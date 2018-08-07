module Metricord.Migrate where

import Import

import System.Directory (listDirectory)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import Database.Persist.Sql

runMigrations :: FilePath -> Handler ()
runMigrations migrationDirectory = do
    migrations <- runDB $ selectList [] [Asc MigrationFilename]
    files <- liftIO $ listDirectory migrationDirectory
    let sqlFiles =
            Set.fromList
            . map Text.pack
            . filter (".sql" `isSuffixOf`)
            $ files
        alreadyRun =
            Set.fromList
            . map (migrationFilename . entityVal)
            $ migrations
        toRun = Set.toList (Set.difference sqlFiles alreadyRun)
    for_ toRun $ \filepath -> do
        contents <- liftIO $ Text.decodeUtf8 <$> BS.readFile (Text.unpack filepath)
        runDB $ do
            rawExecute contents []
            now <- liftIO getCurrentTime
            void $ insert Migration
                { migrationFilename = filepath
                , migrationRun = now
                }
