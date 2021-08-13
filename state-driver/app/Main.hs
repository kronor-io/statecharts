module Main where

import Colog qualified
import Control.Immortal qualified
import Control.Monad.Metrics qualified as Metrics
import Hasql.Connection qualified
import Hasql.OptparseApplicative (poolSettings)
import Hasql.Pool qualified
import Options.Applicative.Simple (
    auto,
    flag,
    help,
    long,
    option,
    short,
    simpleOptions,
    switch,
    value,
 )
import Relude
import Relude.String.Reexport qualified as Text
import Statechart.App
import Statechart.Fetcher qualified
import Statechart.Queue qualified
import StmContainers.Set qualified as ConcurrentSet
import System.Metrics qualified as EKG
import System.Remote.Monitoring qualified as EKG
import UnliftIO.Concurrent (forkIO)
import UnliftIO.STM qualified as STM

data Options = Options
    { numWorkers :: Natural
    , ekgPort :: Int
    , pgPoolSettings :: Hasql.Pool.Settings
    }

main :: IO ()
main = do
    (options, ()) <-
        simpleOptions
            "1.0"
            "state-driver: handles notifications to state machines"
            "Listens to new events for state machines in the database and advances their state"
            ( Options
                <$> option
                    auto
                    ( long "num-workers"
                        <> short 'j'
                        <> help "Number of threads to use for handling new events."
                        <> value 3
                    )
                <*> option
                    auto
                    ( long "ekg-port"
                        <> help "Number of threads to use for handling new events."
                        <> value 3763
                    )
                <*> poolSettings id
            )
            empty

    stateMachines <- atomically ConcurrentSet.new
    inTransit <- atomically ConcurrentSet.new

    ekgStore <- EKG.newStore
    EKG.registerGcMetrics ekgStore
    EKG.forkServerWith ekgStore "0.0.0.0" (ekgPort options)

    let workers = numWorkers options
    let poolConfig = pgPoolSettings options

    metricsStore <- Metrics.initializeWith ekgStore
    Colog.withBackgroundLogger Colog.defCapacity Colog.richMessageAction $ \log ->
        do
            appEnv <- newEnv workers poolConfig log metricsStore
            runApp appEnv do
                Colog.logInfo "Metrics server started at port 3763"
                _ <- Statechart.Queue.runQueue workers stateMachines inTransit
                fetcherThread <- Statechart.Fetcher.runFetcher poolConfig "fsm_machine_events" stateMachines
                liftIO $ Control.Immortal.wait fetcherThread
