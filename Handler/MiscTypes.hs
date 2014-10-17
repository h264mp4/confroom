module Handler.MiscTypes where

import Prelude
import Database.Persist.TH
import Data.Time

data Level = AuthNormal | AuthAdmin | AuthRoot
    deriving (Show, Read, Eq, Ord)
derivePersistField "Level"

-- start and end of a room's booking 
data Timespan = Timespan TimeOfDay TimeOfDay
    deriving (Show, Read, Eq, Ord)
derivePersistField "Timespan"
