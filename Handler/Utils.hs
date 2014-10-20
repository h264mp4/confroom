module Handler.Utils where

import Import

getCurDayAndTime :: IO LocalTime
getCurDayAndTime = do
    timeZ <- getCurrentTimeZone
    utcT  <- getCurrentTime
    return $ utcToLocalTime timeZ utcT

(?) :: Bool -> (a, a) -> a
True  ? (x, _) = x
False ? (_, y) = y
infixl 0 ?