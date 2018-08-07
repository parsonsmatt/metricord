module Model.Role where

import ClassyPrelude

import qualified Data.Text as Text
import Database.Persist.Sql

data Role = View | Insert | Edit
    deriving (Eq, Ord, Show)

instance PersistField Role where
    toPersistValue role = PersistByteString $ case role of
        View -> "View"
        Insert -> "Insert"
        Edit -> "Edit"

    fromPersistValue (PersistByteString bs) =
        case bs of
            "View" -> Right View
            "Insert" -> Right Insert
            "Edit" -> Right Edit
            _ -> Left $ "Expected one of View, Insert or Edit: got " <> Text.pack (show bs)
    fromPersistValue pv =
        Left $ "Expected PersistByteString for Role, got: " <> Text.pack (show pv)

instance PersistFieldSql Role where
    sqlType _ = SqlString
