{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ClassyPrelude

import Data.Function (fix)
import Options.Generic
import Control.Concurrent
import qualified Data.Text as Text

import Metricord
import Metricord.Migrate
import Foundation
import Application

main :: IO ()
main = do
    cmd <- getRecord "metricord-migrate"
    case cmd of
        New {..} ->
            newMigration (defaultDir directory) (unHelpful tableName) (unHelpful field)
        Run {..} -> do
            ctx <- Ctx . appConnPool . snd <$> getFoundation
            flip runReaderT ctx
                . unTaskT
                $ runMigrations (defaultDir directory)

defaultDir :: Maybe FilePath <?> k -> FilePath
defaultDir =
    fromMaybe "./migrations"
    . unHelpful

type DirectoryArg = Maybe FilePath <?> "The location of the SQL migrations. Defaults to ./migrations"

data Cmd
    = New
        { directory :: DirectoryArg
        , tableName :: Text <?> "The name of the table to create."
        , field :: [MigrateColumn] <?> "The fields to seed the table with."
        }
    | Run
        { directory :: DirectoryArg
        }
    deriving (Show, Generic)

instance ParseRecord Cmd
