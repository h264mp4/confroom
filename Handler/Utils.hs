module Handler.Utils where(
       getCurDayAndTime
    )


import Data.Time.Clock
import Data.Time.LocalTime

getCurDayAndTime :: IO LocalTime
getCurDayAndTime = do
    timeZ <- getCurrentTimeZone
    utcT  <- getCurrenTime
    return $ utcToLocalTime timeZ utcT

