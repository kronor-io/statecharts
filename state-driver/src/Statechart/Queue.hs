module Statechart.Queue (runQueue) where

import Colog ((<&))
import Colog qualified
import Control.Immortal.Queue qualified as Queue
import Control.Monad.Metrics qualified as Metrics
import Hasql.Connection qualified
import Hasql.Pool qualified
import Hasql.Session (CommandError (ClientError), QueryError (QueryError))
import ListT qualified
import Relude
import Statechart
import Statechart.App (App, Env (envPgPool), emergencyReplaceConnectionPool, runApp)
import Statechart.Session qualified
import StmContainers.Set qualified as ConcurrentSet
import UnliftIO (async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, throwIO, throwString)
import UnliftIO.STM qualified as STM

runQueue ::
  Natural ->
  ConcurrentSet.Set StateMachine ->
  ConcurrentSet.Set StateMachine ->
  App Queue.QueueId
runQueue threads stateMachines inTransit = do
  env <- ask
  liftIO $ Queue.processImmortalQueue (queueWorker env)
  where
    queueWorker env =
      Queue.ImmortalQueue
        { Queue.qThreadCount = threads,
          Queue.qPollWorkerTime = 20,
          Queue.qPop = atomically (getTask stateMachines inTransit),
          Queue.qPush = runApp env . insertTask,
          Queue.qHandler = runApp env . runStateMachine inTransit,
          Queue.qFailure = \item e -> runApp env $ handleFailure item e
        }

    insertTask :: HasCallStack => StateMachine -> App ()
    insertTask item = atomically do
      ConcurrentSet.insert item stateMachines

    handleFailure :: (HasCallStack, Exception e) => StateMachine -> e -> App ()
    handleFailure item exception = do
      Metrics.increment "queue.exception"
      Colog.logError $ "Error processing item: " <> show item
      Colog.logException exception

      void $ async do
        threadDelay 1_000_000 -- wait second and re-queue
        atomically do
          ConcurrentSet.insert item stateMachines

getTask ::
  ConcurrentSet.Set StateMachine ->
  ConcurrentSet.Set StateMachine ->
  STM StateMachine
getTask stateMachines inTransit = do
  numElems <- ConcurrentSet.size stateMachines
  STM.checkSTM (numElems > 0)

  pending <- getNextStateMachine

  case pending of
    Nothing -> STM.retrySTM
    Just machine -> do
      ConcurrentSet.delete machine stateMachines
      return machine
  where
    getNextStateMachine :: STM (Maybe StateMachine)
    getNextStateMachine = do
      machine <- ListT.foldMaybe isSuitable Nothing (ConcurrentSet.listT stateMachines)

      -- We need to mark the machine as in transit once it is selected, so that
      -- no other thread can try to execute the same machine
      case machine of
        Just m -> ConcurrentSet.insert m inTransit
        Nothing -> return ()

      return machine

    isSuitable (Just _) _ = pure Nothing -- machine was selected in previous step, signal stop
    isSuitable Nothing candidate = do
      -- no machine has been selected, check if it is suitable
      isCurrentlyInTransit <- ConcurrentSet.lookup candidate inTransit
      return $
        if isCurrentlyInTransit
          then pure Nothing -- singal continuation, but with no machine selected yet
          else pure (Just candidate) -- signal continuation, with a machine selected

-- | Actually run the state machine in the database
runStateMachine :: HasCallStack => ConcurrentSet.Set StateMachine -> StateMachine -> App ()
runStateMachine inTransit stateMachine = do
  Colog.logDebug $ "Processing machine: " <> show (stateMachineId stateMachine)

  result <-
    bracket
      signalWorkingOn
      signalFinished
      runMachine

  case result of
    Left (Hasql.Pool.SessionError (QueryError _ _ (ClientError (Just err)))) -> do
      Metrics.increment "queue.session_error"
      env <- ask
      pool <- STM.readTVarIO (envPgPool env)

      when ("no connection to the server\n" == err) do
        Metrics.increment "queue.connection_error"
        emergencyReplaceConnectionPool pool

      -- we need to throw an exception to let the queue worker
      -- put the item back in the queue
      throwString (decodeUtf8 err)
    Left e -> do
      Metrics.increment "queue.generic_error"
      throwString (show e)
    Right _ -> do
      Metrics.increment "queue.processed"
      Colog.logDebug "Done processing item"
  where
    signalWorkingOn = do
      Metrics.gaugeIncrement "queue.processing"
      atomically do
        ConcurrentSet.insert stateMachine inTransit

    signalFinished _ = do
      Metrics.gaugeDecrement "queue.processing"
      atomically do
        ConcurrentSet.delete stateMachine inTransit

    runMachine _ = do
      env <- ask
      pool <- STM.readTVarIO (envPgPool env)
      Metrics.timed' Metrics.Milliseconds "queue.handle_time" do
        liftIO do
          Hasql.Pool.use pool do
            Statechart.Session.handleMachineEvents stateMachine
