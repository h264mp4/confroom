{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(fromJust)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

{-  uid <- runDB $ getUserIdByUniqueUserEmail "peng@123.com"
    case uid of
                   -- lift io to the current monad layer
        Nothing -> liftIO $ print "Nothing to be delete, cannot find peng@123.com"
        Just theId -> runDB $ deleteUser theId
-}


testUser = User "peng_pxt@163.com" "hah" "peng" AuthNormal False "waht" "wahtandwhat"
testRoom t1 t2 = Room "1001" True t1 t2 AuthNormal

testBookingRoom aDay curTime t1 = do
    mayUserId <- runDB $ addNewUser testUser
    mayRoomId <- runDB $ addNewRoom (testRoom t1 t1)
    runDB $ bookingRoom (fromJust mayUserId) (fromJust mayRoomId) aDay (Timespan curTime curTime)
    

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text

    curDT <- liftIO getCurDayAndTime
    let curDay = localDay curDT
        curTime = localTimeOfDay curDT

    t1 <- liftIO $ getCurrentTime

    --theId <- testBookingRoom curDay curTime t1
    --runDB $ cancelABooking theId

    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        --addScript $ StaticR kalendae_kalendae_standalone_js 
        --addStylesheet $ StaticR kalendae_kalendae_css
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing


--    <link rel="stylesheet" href="build/kalendae.css" type="text/css" charset="utf-8">
--    <script src="build/kalendae.standalone.js" type="text/javascript" charset="utf-8"></script>