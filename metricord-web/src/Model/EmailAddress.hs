-- | A wrapper around the EmailAddress type from the "email-validate"
-- library.
module Model.EmailAddress where

import ClassyPrelude

import Data.Bifunctor (bimap)
import GHC.Generics (Generic)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.Persist.Sql
import qualified Text.Email.Validate as EV

newtype EmailAddress
    = EmailAddress
    { unEmailAddress :: EV.EmailAddress
    }
    deriving newtype (Eq, Show, Ord, Generic)

instance PersistField EmailAddress where
    toPersistValue = PersistByteString . EV.toByteString . unEmailAddress
    fromPersistValue pv =
        case pv of
            PersistByteString bs ->
                bimap Text.pack EmailAddress (EV.validate bs)
            PersistText txt ->
                bimap Text.pack EmailAddress (EV.validate (Text.encodeUtf8 txt))
            _ ->
                Left $
                    "Expected a Text or ByteString, got: "
                    <> Text.pack (show pv)


instance PersistFieldSql EmailAddress where
    sqlType _ = SqlString
