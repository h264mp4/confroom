module Model where

import Yesod
import Data.Text (Text)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Data.ByteString (ByteString)
import Data.Time
import Handler.MiscTypes
import Prelude

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
