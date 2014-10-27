module Handler.MiscTypes where

import Prelude
import Database.Persist.TH
import Data.Time

data Level = AuthNormal | AuthAdvance |AuthAdmin 
    deriving (Read, Eq, Ord)
derivePersistField "Level"

instance Show Level where
    show AuthNormal  = "普通 "
    show AuthAdvance = "领导"
    show AuthAdmin   = "管理员"

-- start and end of a room's booking 
data Timespan = Timespan TimeOfDay TimeOfDay
    deriving (Show, Read, Eq, Ord)
derivePersistField "Timespan"
