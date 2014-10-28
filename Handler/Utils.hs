module Handler.Utils where

import Import
import Handler.MiscTypes

getCurDayAndTime :: IO LocalTime
getCurDayAndTime = do
    timeZ <- getCurrentTimeZone
    utcT  <- getCurrentTime
    return $ utcToLocalTime timeZ utcT

convertUtcToZoneTime utcT = do
    timeZ <- getCurrentTimeZone
    return $ utcToZonedTime timeZ utcT

(?) :: Bool -> (a, a) -> a
True  ? (x, _) = x
False ? (_, y) = y
infixl 0 ?

authLevel :: [(Text, Level)]
authLevel = [("普通", AuthNormal), ("领导", AuthAdvance),("管理员", AuthAdmin)]
