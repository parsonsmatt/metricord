module Metricord.Migrate where

import Import

import Data.Function hiding ((.))
import Options.Generic
import System.Directory (listDirectory)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import Database.Persist.Sql
import Text.Read as Read
import Data.Time
import Data.Time.Format

import Metricord

runMigrations :: FilePath -> Task ()
runMigrations migrationDirectory = do
    migrations <- runDb $ selectList [] [Asc MigrationFilename]
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
        runDb $ do
            rawExecute contents []
            now <- liftIO getCurrentTime
            void $ insert Migration
                { migrationFilename = filepath
                , migrationRun = now
                }

data MigrateColumn = MigrateColumn Text Text
    deriving (Eq)

instance Show MigrateColumn where
    show (MigrateColumn name typ) = Text.unpack name <> ":" <> Text.unpack typ

instance Read MigrateColumn where
    readPrec = do
        name <- fix $ \loop -> do
            c <- Read.get
            case c of
                ':' -> pure []
                _ -> (c :) <$> loop
        typ <- fix $ \loop -> do
            mc <- fmap Just Read.get +++ pure Nothing
            case mc of
                Just c -> (c :) <$> loop
                Nothing -> pure []
        pure ((MigrateColumn `on` Text.pack) name typ)

instance ParseField MigrateColumn

newMigration :: FilePath -> Text -> [MigrateColumn] -> IO ()
newMigration directory tableName fields = do
    now <- getCurrentTime
    let contents =
            constructSql tableName fields
        filename =
            formatTime defaultTimeLocale "%F_%H%M%S-" now
            ++ "create-" ++ Text.unpack tableName ++ ".sql"
    BS.writeFile (directory </> filename) (Text.encodeUtf8 contents)

constructSql :: Text -> [MigrateColumn] -> Text
constructSql tableName fields =
    "CREATE TABLE " <> tableName <> " (\n" <>
    "    id      SERIAL,\n" <>
    constructFields fields <>
    "\n);"

constructFields :: [MigrateColumn] -> Text
constructFields =
    Text.intercalate ",\n"
    . map (\(MigrateColumn name typ) -> "    " <> name <> "    " <> typ <> " NOT NULL")

