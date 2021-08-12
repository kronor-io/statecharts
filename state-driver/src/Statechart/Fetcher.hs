module Statechart.Fetcher where

import Colog qualified
import Control.Immortal qualified
import Control.Monad.Metrics qualified as Metrics
import Data.Aeson qualified as Json
import Data.Vector qualified as Vector
import Hasql.Connection qualified
import Hasql.Session qualified
import Relude
import Statechart (StateMachine)
import Statechart.App (App)
import Statechart.Listener qualified
import Statechart.Session qualified
import StmContainers.Set qualified as ConcurrentSet
import UnliftIO (async, wait)
import UnliftIO.Async (cancel, link)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception qualified as Exception

newtype ConnectionError = ConnectionError {connectionErrorMessage :: String}
  deriving stock (Show)

instance Exception ConnectionError

runFetcher ::
  Hasql.Connection.Settings ->
  Text ->
  ConcurrentSet.Set StateMachine ->
  App Control.Immortal.Thread
runFetcher settings channel stateMachines =
  Control.Immortal.createWithLabel "fetcher" $
    \thread ->
      Control.Immortal.onUnexpectedFinish thread logException do
        Exception.bracket acquire2 release fetch
  where
    acquire2 = do
      db1OrError <- liftIO $ Hasql.Connection.acquire settings
      db2OrError <- liftIO $ Hasql.Connection.acquire settings
      case (db1OrError, db2OrError) of
        (Left e, _) -> Exception.throwIO $ ConnectionError ("Unable to establish database connection: " <> show e)
        (_, Left e) -> Exception.throwIO $ ConnectionError ("Unable to establish database connection: " <> show e)
        (Right db1, Right db2) -> return (db1, db2)

    release (db1, db2) = liftIO do
      Hasql.Connection.release db1
      Hasql.Connection.release db2

    fetch (db1, db2) = do
      Colog.logInfo "Listening to machine events"
      isReadyVar <- newEmptyTMVarIO
      -- running in a different thread because we need to start the listener before
      -- we fetch old events, to ensure we don't miss any
      threadID <- async do
        Statechart.Listener.runListener db1 isReadyVar channel stateMachines
      link threadID

      -- we need to wait for the listener to begin its work before fetching pending events
      -- so that we leave no gap in between the time it starts and the time we fetch unhandled
      -- events.
      _ <- atomically $ takeTMVar isReadyVar
      Colog.logInfo "Fetching pending events"
      maybeMachines <- liftIO $ Hasql.Session.run Statechart.Session.getPendingMachines db2

      case maybeMachines of
        Left e -> do
          Metrics.increment "fetcher.query_error"
          cancel threadID
          Exception.throwIO e
        Right pendingMachines -> do
          Colog.logInfo $ "I found this many: " <> show (Vector.length pendingMachines)
          Metrics.counter "fetcher.pending_machines" (Vector.length pendingMachines)
          atomically do
            traverse_ (`ConcurrentSet.insert` stateMachines) pendingMachines

      liftIO $ Hasql.Connection.release db2
      wait threadID

    logException (Left e) = do
      Metrics.increment  "fetcher.exception"
      Colog.logException e
      Colog.logDebug "Trying again after one second"
      threadDelay (1 * 1_000_000)
    logException _ = return ()
