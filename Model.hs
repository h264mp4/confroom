module Model where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Prelude


data Level = Normal | Admin | Root
    deriving (Show,Read, Eq, Ord)
derivePersistField "Level"

-- start and end of a room's booking 
data Timespan = Timespan TimeOfDay TimeOfDay

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
