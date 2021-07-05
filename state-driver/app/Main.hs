module Main where

import Colog qualified
import Control.Immortal qualified
import Control.Monad.Metrics qualified as Metrics
import Hasql.Connection qualified
import Hasql.Pool qualified
import Relude
import Statechart.App
import Statechart.Fetcher qualified
import Statechart.Queue qualified
import StmContainers.Set qualified as ConcurrentSet
import System.Remote.Monitoring qualified as EKG
import System.Metrics qualified as EKG
import UnliftIO.STM qualified as STM
import UnliftIO.Concurrent (forkIO)

connString :: Hasql.Connection.Settings
connString = "postgres://statecharts:statecharts@localhost/statecharts"

numWorkers :: Natural
numWorkers = 3

main :: IO ()
main = do
  stateMachines <- atomically ConcurrentSet.new
  inTransit <- atomically ConcurrentSet.new

  ekgStore <- EKG.newStore
  EKG.registerGcMetrics ekgStore
  EKG.forkServerWith ekgStore "0.0.0.0" 3763

  metricsStore <- Metrics.initializeWith ekgStore
  Colog.withBackgroundLogger Colog.defCapacity Colog.richMessageAction $ \log ->
    do
      appEnv <- newEnv numWorkers connString log metricsStore
      runApp appEnv do
        Colog.logInfo "Metrics server started at port 3763"
        _ <- Statechart.Queue.runQueue numWorkers stateMachines inTransit
        fetcherThread <- Statechart.Fetcher.runFetcher connString "fsm_machine_events" stateMachines
        liftIO $ Control.Immortal.wait fetcherThread
