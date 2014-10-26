{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.AddRoom where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))


getAddRoomR :: Handler Html
getAddRoomR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getAddRoomR" :: Text

    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "addroom")

postAddRoomR :: Handler Value
postAddRoomR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "postAddRoomR" :: Text

    req <- waiRequest
    req' <- getRequest
    liftIO $ print req
    liftIO $ print "Get request body json"
    !res <- runRequestBody
    liftIO $ print $ fst res
    liftIO $ print $ reqLangs req'
    return $ object [ ("sEcho" :: Text) .= (1 :: Int) ]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
