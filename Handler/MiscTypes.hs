module Handler.MiscTypes where

import Prelude
import Database.Persist.TH
import Data.Time

data Level = AuthNormal | AuthAdvance | AuthAdmin 
    deriving (Show, Read, Eq, Ord)
derivePersistField "Level"

toLevelString lev
    | lev == AuthNormal  = "普通 "
    | lev == AuthAdvance = "领导"
    | lev == AuthAdmin   = "管理员"
    | otherwise          = "Wrong Level"

-- start and end of a room's booking 
data Timespan = Timespan TimeOfDay TimeOfDay
    deriving (Show, Read, Eq, Ord)
derivePersistField "Timespan"
