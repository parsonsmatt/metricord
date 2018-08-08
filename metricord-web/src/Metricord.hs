{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Metricord where

import Import

import Control.Lens
import Data.Generics.Product

import Database.Persist.Sql

newtype TaskT m a = TaskT
    { unTaskT :: ReaderT Ctx m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Ctx, MonadIO)

data Ctx = Ctx
    { ctxConnPool :: !ConnectionPool
    } deriving Generic

instance (MonadUnliftIO m) => MonadUnliftIO (TaskT m) where
    askUnliftIO =
        TaskT
        $ withRunInIO
        $ \run -> pure (UnliftIO (run . unTaskT))

type Task = TaskT IO

runDb
    :: (MonadReader r (t m), MonadTrans t, MonadUnliftIO m, HasType ConnectionPool r)
    => SqlPersistT m a
    -> t m a
runDb query = do
    conn <- asks  (\s -> s ^. typed @ConnectionPool)
    lift $ runSqlPool query conn
