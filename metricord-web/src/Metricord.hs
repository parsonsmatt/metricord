{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Metricord where

import Import

import Database.Persist.Sql

newtype TaskT m a = TaskT
    { unTaskT :: ReaderT App m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadReader App, MonadIO)

instance (MonadUnliftIO m) => MonadUnliftIO (TaskT m) where
    askUnliftIO =
        TaskT
        $ withRunInIO
        $ \run -> pure (UnliftIO (run . unTaskT))

type Task = TaskT IO

runDb :: (MonadReader App (t m), MonadTrans t, MonadUnliftIO m) => SqlPersistT m a -> t m a
runDb query = do
    conn <- asks appConnPool
    lift $ runSqlPool query conn
