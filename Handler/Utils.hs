module Handler.Utils where

import Import
import Handler.MiscTypes
import System.IO.Unsafe(unsafePerformIO)

myTimeZone = unsafePerformIO $ getCurrentTimeZone

getCurDayAndTime :: IO LocalTime
getCurDayAndTime = do
    timeZ <- getCurrentTimeZone
    utcT  <- getCurrentTime
    return $ utcToLocalTime timeZ utcT

convertUtcToZoneTime = utcToZonedTime myTimeZone

(?) :: Bool -> (a, a) -> a
True  ? (x, _) = x
False ? (_, y) = y
infixl 0 ?

authLevel :: [(Text, Level)]
authLevel = [("普通", AuthNormal), ("领导", AuthAdvance),("管理员", AuthAdmin)]
