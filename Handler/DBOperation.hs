{-# LANGUAGE OverloadedStrings #-}
module Handler.DBOperation where

import Data.Time
import Network.Mail.SMTP
import Data.Maybe(fromJust)
import Database.Persist.Sqlite
import qualified Data.Text as T

import Import
import Handler.Utils
import Handler.MiscTypes

-- | All basic operations are defined here and will called by Handler or Widget.

------------------------------------------------------------------------------------------
-- | User Operaions: addNewUser editUserProfile deleteUser

-- addNewUser :: User -> | TODO: WHAT's the return type??
addNewUser newUser = runSqlite ":memory:" $ do
    --runMigration migrateAll
    userExist <- selectList [UserEmail ==. (userEmail newUser)] [LimitTo 1]
    if not (null userExist)
       then do let errMsg = "User" ++ (show $ userEmail newUser) ++
                              "Already Exists, should use others"
               liftIO $ print errMsg
               return ()
       else do
            newUserId <- insert newUser
            return ()

editUserProfile theUserId newInfo = runSqlite ":memory:" $ do
    -- only name, password and level can be editted.
    update theUserId [ UserName     =. (userName newInfo), 
                       UserPassword =. (userPassword newInfo),
                       UserLevel    =. (userLevel newInfo)
                     ]
    return ()

deleteUser theUserId = runSqlite ":memory:" $ do
    delete theUserId
    return ()

------------------------------------------------------------------------------------------
-- | Room operations: addNewRoom, editRoomProfile, deleteRoom
addNewRoom newRoom = runSqlite ":memory:" $ do
    roomExist <- selectList [RoomNumber ==. (roomNumber newRoom)] [LimitTo 1]
    if not (null roomExist)
       then do let errMsg = "Room" ++ (show $ roomNumber newRoom) ++
                              "Already Exists, room configuration can be editted"
               liftIO $ print errMsg
               return ()
       else do
            newRoomId <- insert newRoom
            return ()

editRoomProfile theRoomId newInfo = runSqlite ":memory:" $ do
    -- Only the following field can be changed
    update theRoomId [ RoomAvailable =. (roomAvailable newInfo), 
                       RoomValidTime =. (roomValidTime newInfo),
                       RoomLevel     =. (roomLevel newInfo)
                     ]
    return "OK"

deleteRoom theRoomId = runSqlite ":memory:" $ do
    delete theRoomId
    return ()

------------------------------------------------------------------------------------------
-- | Booking management: bookingRoom,  deleteABooking

data BookingStatus = BookingCancel | BookingSuccess

-- | TODO: we should check timespan overlapping
bookingRoom theUserId theRoomId timespan = runSqlite ":memory:" $ do
    curDT <- liftIO $ getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT
        newRecord = Record theUserId theRoomId curDay timespan curTime False
    newRecordId <- insert newRecord
    maybeUser <- get theUserId
    maybeRoom <- get theRoomId
    -- email the user
    liftIO $ emailNotification newRecord (fromJust maybeUser) 
                               (fromJust maybeRoom) BookingSuccess 

    -- we add this new recordId to DayRecords
    maybeDay <- getBy $ UniqueDay curDay
    case maybeDay of
        Nothing -> do
                   insert $ DayRecords [newRecordId] curDay
                   return ()
        Just (Entity existId records) -> do
            let existRecords  = dayRecordsIds records
                updateRecords = existRecords ++ [newRecordId]
            update existId [DayRecordsIds =. updateRecords]
            return ()

-- TODO: return type??
cancelABooking recordId = runSqlite ":memory:" $ do
    maybeRecord <- get recordId
    case maybeRecord of
        Nothing -> return ()
        Just aRecord -> do 
              update recordId [RecordCancel =. True]
              let aUserId = recordUserId aRecord
                  aRoomId = recordRoomId aRecord
              maybeUser <- get aUserId          
              maybeRoom <- get aRoomId
              liftIO $ emailNotification aRecord (fromJust maybeUser) 
                                         (fromJust maybeRoom) BookingCancel
              return ()

------------------------------------------------------------------------------------------
-- | Email Operations: sending booking/cancel emails
emailNotification :: Record -> User -> Room -> BookingStatus -> IO ()
emailNotification aRecord aUser aRoom status = do
    let adminUserName = "testConfroom" -- 163 username and passphrase
        adminPassword = "testConfroom123"
        viaHost       = T.pack "smtp.163.com"
        viaPort       = T.pack $ show 25

-- formatTime defaultTimeLocale "%F | %T" x 

    let emailText  = userEmail aUser
        nameText   = userName  aUser
        roomText   = roomNumber aRoom
        theDay     = recordDay aRecord
        timespan   = recordTimespan aRecord
        statusText = getStatusText status
        (year, month, day)   = toGregorian theDay
        (startTime, endTime) = timeSpanToTimeString timespan

    -- create the email
    let fromAddress = Address (Just $ T.pack "Conference Room Admin") 
                              (T.pack "testConfromm@163.com")
        toAddress   = [Address (Just nameText) emailText]
        ccAddress   = []
        bccAddress  = []
        subject     = T.pack (statusText !! 0 ++ ": " ++ "Conference Room " ++ 
                              T.unpack roomText ++ ", " ++ startTime ++ "--" ++ endTime)
        allParts    = plainTextPart (T.pack "This is a test")
        theMail     = simpleMail fromAddress toAddress ccAddress bccAddress subject allParts

    sendMailWithLogin' viaHost viaPort adminUserName adminPassword theMail 
    return ()  

getStatusText status | status == BookingSuccess = ["Booking Success Notification", ""]
                     | status == BookingCancel  = ["Booking Cancel Notification", ""]
                     | otherwise = ["",""]

timeSpanToTimeString (Timespan start end) = 
                    let startHour = todHour start 
                        startMin  = todMin start
                        endHour = todHour end 
                        endMin  = todMin end
                     in (show startHour ++ ":" ++ show startMin,
                         show endHour ++ ":" ++ show endMin)