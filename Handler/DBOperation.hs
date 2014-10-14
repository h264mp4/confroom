{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.DBOperation where

import Import
import Data.Time.Clock
import Network.Mail.Smtp
import Data.Maybe(fromJust)

-- | All basic operations are defined here and will called by Handler or Widget.

------------------------------------------------------------------------------------------
-- | User Operaions: addNewUser editUserProfile deleteUser

-- addNewUser :: User -> | TODO: WHAT's the return type??
addNewUser newUser = runSqlite $ do
    --runMigration migrateAll
    userExist <- selectList [UserEmail ==. (userEmail newUser)] [LimitTo 1]
    if not null userExist
       then do let errMsg = "User" ++ (show $ userEmail newUser) ++
                              "Already Exists, should use others"
               liftIO $ print errMsg
               return errMsg
       else do
            newUserId <- insert newUser
            return "OK"

editUserProfile theUserId newInfo = runSqlite $ do
    -- only name, password and level can be editted.
    update theUserId [ UserName     =. (userName newInfo), 
                       UserPassword =. (userPassword newInfo),
                       UserLevel    =. (userLevel newInfo)
                     ]
    return "OK"

deleteUser theUserId = runSqlite $ do
    delete theUserId
    return "OK"

------------------------------------------------------------------------------------------
-- | Room operations: addNewRoom, editRoomProfile, deleteRoom
addNewRoon newRoom = runSqlite $ do
    roomExist <- selectList [RoomNumber ==. (roomNumber newRoom)] [LimitTo 1]
    if not null userExist
       then do let errMsg = "Room" ++ (show $ userEmail newUser) ++
                              "Already Exists, room configuration can be editted"
               liftIO $ print errMsg
               return errMsg
       else do
            newRoomId <- insert newRoom
            return "OK"

editRoomProfile theRoomId newInfo = runSqlite $ do
    -- Only the following field can be changed
    update theRoomId [ RoomAvailable =. (roomAvailable newInfo), 
                       RoomValidTime =. (roomValidTime newInfo),
                       RoomLevel     =. (roomLevel newInfo)
                     ]
    return "OK"

deleteRoom theRoomId = runSqlite $ do
    delete theRoomId
    return "OK"

------------------------------------------------------------------------------------------
-- | Booking management: bookingRoom,  deleteABooking

data BookingStatus = BookingCancel | BookingSuccess

bookingRoom theUserId theRoomId timespan = runSqlite $ do
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
        Nothing -> insert $ DayRecords [newRecordId] curDay >> return ()
        Just (Entity existId records) -> do
            let existRecords  = dayRecordsIds records
                updateRecords = existRecords ++ [newRecordId]
            update existId [recordIds =. updateRecords] >> return ()

-- TODO: return type??
cancelABooking recordId = runSqlite $ do
    maybeRecord <- get recordId
    case maybeRecord of
        Nothing -> return ()
        Just aRecord -> do 
              update recordId [RecordCancel =. True]
              let aUserId = recordUserId aRecord
                  aRoomId = recordRoomId aRecord
              maybeUser <- get aUserId          
              maybeRoom <- get aRoomId
              liftIO $ emailCancelNotification aRecord (fromJust maybeUser) 
                                               (fromJust maybeRoom) BookingCancel
              return ()

------------------------------------------------------------------------------------------
-- | Email Operations: sending booking/cancel emails
emailNotification :: Record -> User -> Room -> BookingStatus -> IO ()
emailNotification aRecord aUser aRoom status = do
    let adminUserName = "testConfroom" -- 163 username and passphrase
        adminPassword = "testConfroom123"
        viaHost       = "smtp.163.com"
        viaPort       = 25

    let emailText = userEmail aUser
        nameText  = userName  aUser
        roomText  = roomNumber aRoom
        date      = recordDay aRecord
        timespan  = recordTimespan aRecord
        statusText = getStatusText status

    -- create the email
    let fromAddress = Address (Just "Conference Room Admin") (show viaPort)
        toAddress   = [emailText]
        ccAddress   = []
        bccAddress  = []
        subject     = statusText !! 0 ++ ": " ++ "Conference Room " ++ roomText
 
    sendmailWithLogin' viaHost viaPort adminUsername adminPassword theMail 
    return ()  

getStatusText status | status == BookingSuccess = ["Booking Success Notification", ""]
                     | status == BookingCancel  = ["Booking Cancel Notification", ""]
User
    email Text
    password ByteString
    name Text
    level Level
    verified Bool
    verifyKey Text
    resetPasswordKey Text
    UniqueEmail email
    deriving Show

Room
    number  Text
    available Bool
    firstAdd UTCTime
    validTime UTCTime
    level Level
    UniqueRoomNo number
    deriving Show

Record
    userId UserId
    roomId RoomId
    day Day
    timespan Timespan
    bookingTime TimeOfDay
    cancel Bool
    deriving Show

        
