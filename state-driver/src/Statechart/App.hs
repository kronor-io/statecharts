module Statechart.App where

import Colog qualified
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Metrics qualified as Metrics
import Hasql.Connection qualified
import Hasql.Pool qualified
import Relude
import UnliftIO (MonadUnliftIO, async)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM qualified as STM

data Env m = Env
    { envLogAction :: Colog.LogAction m Colog.Message
    , envPoolSettings :: Hasql.Pool.Settings
    , envPgPool :: STM.TVar Hasql.Pool.Pool
    , envMetrics :: Metrics.Metrics
    }

instance Colog.HasLog (Env m) Colog.Message m where
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction newLogAction env = env{envLogAction = newLogAction}
    {-# INLINE setLogAction #-}

newtype App a = App
    { -- | Run the application using the ReaderT design pattern
      unApp :: ReaderT (Env App) IO a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Env App)
        , MonadUnliftIO
        , MonadThrow
        , MonadCatch
        , MonadMask
        )

-- | Implement the metrics monad for App. Not explicitly using App, but ReaderT,
-- in case we want to break capabilities further. Remember that App is (ReaderT (Env App) ..)
-- so the m here will match with App.
instance Metrics.MonadMetrics App where
    getMetrics = asks envMetrics
    {-# INLINE getMetrics #-}

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (coerce app) env

newEnv ::
    Natural ->
    Hasql.Pool.Settings ->
    Colog.LogAction m Colog.Message ->
    Metrics.Metrics ->
    IO (Env m)
newEnv poolSize connectionSettings logger metrics = do
    pool <- makePgPool connectionSettings
    poolVar <- STM.newTVarIO pool
    return $ Env logger connectionSettings poolVar metrics

-- | This is a workaround for a problem with hasql-pool, that it
-- puts closed connection objects back in the pool. We are detroying
-- here the old pool and replacing it with a fresh one. This will kill
-- any in-flight query
emergencyReplaceConnectionPool :: Hasql.Pool.Pool -> App ()
emergencyReplaceConnectionPool badPool = do
    env <- ask
    pool <- makePgPool (envPoolSettings env)

    atomically do
        STM.writeTVar (envPgPool env) pool

    void $ async do
        -- wait 5 seconds before closing the pool.
        -- we do this because we want to give a chance to other connections
        -- to finish their work in case they are still healthy
        threadDelay (5 * 1_000_000)
        liftIO $ Hasql.Pool.release badPool

makePgPool ::
    MonadIO m =>
    Hasql.Pool.Settings ->
    m Hasql.Pool.Pool
makePgPool connectionSettings = liftIO do
    Hasql.Pool.acquire connectionSettings
