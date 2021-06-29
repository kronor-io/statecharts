module Statechart.Listener where

import Control.Monad.Metrics qualified as Metrics
import Data.Aeson qualified as Json
import Hasql.Connection qualified
import Hasql.Notifications qualified
import Relude
import Statechart.App (App, runApp)
import StmContainers.Set qualified as ConcurrentSet
import UnliftIO.Exception qualified as Exception

newtype DecodingError = DecodingError {decodingErrorMessage :: String}
  deriving stock (Show)

instance Exception DecodingError

runListener ::
  (Json.FromJSON item, Eq item, Hashable item) =>
  -- | Postgres connection handler
  Hasql.Connection.Connection ->
  -- | Mvar to signal that the listener is setup and working
  TMVar () ->
  -- | The postgres channel name to listen to
  Text ->
  -- | The Set where work items should be stored for future processing
  ConcurrentSet.Set item ->
  App ()
runListener db isReadyVar channel stateMachines = do
  let channelToListen = Hasql.Notifications.toPgIdentifier channel

  liftIO do
    Hasql.Notifications.listen db channelToListen

  -- signal that we are already listening to events
  atomically $ putTMVar isReadyVar ()

  env <- ask
  liftIO do
    Hasql.Notifications.waitForNotifications (notifier env) db
  where
    notifier env _ payload = runApp env (notificationHandler payload)

    notificationHandler payload = do
      Metrics.increment "state_machine_event.received"

      case Json.eitherDecode (toLazy payload) of
        Right machine -> atomically do
          ConcurrentSet.insert machine stateMachines
        Left err -> do
          Metrics.increment "state_machine_event.json_error"
          -- this would be a pretty serious thing, logging is not enough
          Exception.throwIO $ DecodingError ("error decoding payload in notification: " <> err)
