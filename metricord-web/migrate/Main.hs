{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ClassyPrelude

import Data.Function (fix)
import Options.Generic
import Control.Concurrent
import qualified Data.Text as Text

import Metricord.Migrate

main :: IO ()
main = do
    cmd <- getRecord "metricord-migrate"
    case cmd of
        New {..} -> do
            let dir =
                    fromMaybe "./migrations"
                    . unHelpful
                    $ directory
            newMigration dir (unHelpful tableName) (unHelpful field)
        Run {..} ->
            putStrLn "Not implemented yet"

    print cmd

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
