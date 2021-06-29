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
        Exception.bracket acquire release fetch
  where
    acquire = do
      dbOrError <- liftIO $ Hasql.Connection.acquire settings
      case dbOrError of
        Left e -> Exception.throwIO $ ConnectionError ("Unable to establish database connection: " <> show e)
        Right db -> return db

    release db = liftIO do
      Hasql.Connection.release db

    fetch db = do
      Colog.logInfo "Listening to machine events"
      isReadyVar <- newEmptyTMVarIO
      -- running in a different thread because we need to start the listener before
      -- we fetch old events, to ensure we don't miss any
      threadID <- async do
        Statechart.Listener.runListener db isReadyVar channel stateMachines
      link threadID

      Colog.logInfo "Fetching pending events"
      -- we need to wait for the listener to begin its work before fetching pending events
      -- so that we leave no gap in between the time it starts and the time we fetch unhandled
      -- events.
      _ <- atomically $ takeTMVar isReadyVar
      maybeMachines <- liftIO $ Hasql.Session.run Statechart.Session.getPendingMachines db

      case maybeMachines of
        Left e -> do
          Metrics.increment "fetcher.query_error"
          cancel threadID
          Exception.throwIO e
        Right pendingMachines -> do
          Metrics.counter "fetcher.pending_machines" (Vector.length pendingMachines)
          atomically do
            traverse_ (`ConcurrentSet.insert` stateMachines) pendingMachines

      wait threadID

    logException (Left e) = do
      Metrics.increment  "fetcher.exception"
      Colog.logException e
      Colog.logDebug "Trying again after one second"
      threadDelay (1 * 1_000_000)
    logException _ = return ()
