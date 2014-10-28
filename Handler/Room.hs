{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Room where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(fromJust)
import Data.Aeson(ToJSON(..), object, (.=))
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 

------------------------------------------------------------------------------------------
---- AddRoom

getAddRoomR :: Handler Html
getAddRoomR = do
    (addRoomWidget, formEnctype) <- generateFormPost addRoomForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getAddRoomR" :: Text

    defaultLayout $ do
        addRoomFormId <- newIdent
        $(widgetFile "addroom")

postAddRoomR :: Handler Html
postAddRoomR = do
    ((result, formWidget), formEnctype) <- runFormPost addRoomForm
    let handlerName = "postAddRoomR" :: Text
    case result of
        FormSuccess formInfo -> do
            mayRoomId <- runDB $ addNewRoom formInfo
            case mayRoomId of
                 Nothing -> defaultLayout $ backWidget  ("会议室信息已存在，请重新输入" :: String)

                 Just roomId -> do
                     liftIO $ print ("Add new room done: " ++ show (fromJust mayRoomId))
                     liftIO $ print formInfo
                     defaultLayout $ backWidget (toHtmlRoomInfo formInfo)
        _ -> defaultLayout $ backWidget ("无效的会议室信息, 请重新输入." :: String)

    where
    backWidget info = toWidget [hamlet|
                            <div class="row">
                                <div class="col-md-12">
                                    <input type=button value="返回" class="btn btn-primary" onClick="location.href='@{AddRoomR}'">
                                <div class="col-md-12">
                                     <h3> 会议室信息已保存
                                <div class="col-md-12"> 
                                    <p> #{preEscapedToMarkup info}                     
|]

simpleFormLayoutForAddRoom = BootstrapHorizontalForm
                             {
                                  bflLabelOffset = ColMd 0
                                 ,bflLabelSize   = ColMd 4
                                 ,bflInputOffset = ColMd 0
                                 ,bflInputSize   = ColMd 4
                             }

addRoomForm :: Form Room
addRoomForm = renderBootstrap3 simpleFormLayoutForAddRoom $ Room
        <$> areq textField "会议室编号" Nothing
        <*> areq (selectFieldList authLevel) "预订权限" Nothing
        <*> areq boolField "是否现在启用" (Just True)
        <*> areq (jqueryDayField def {jdsChangeMonth = True, jdsChangeYear = True}) 
                                 "会议室有效期至" Nothing
        <*> lift (liftIO $ getCurrentTime)

------------------------------------------------------------------------------------------
---- list room
-- fakeJsonRet = object $ [fakeDataName .= fakeDataRows, "total" .= toJSON (4::Int)]
--    return $ fakeJsonRet
getListRoomR :: Handler Value
getListRoomR = do
    -- TODO: User Auth Widget
    rooms <- runDB $ listRoomProfile
    if null rooms
       then return $ object $ []
       else do
            return $ object $ ["dataRows" .= (map toJSON rooms), 
                               "total" .= toJSON (length rooms :: Int)
                              ]
   
-- editRoomR theId = do


------------------------------------------------------------------------------------------
---- other helpers

toHtmlRoomInfo :: Room -> String
toHtmlRoomInfo roomInfo = (
    "会议室编号: " ++ (show $ roomNumber roomInfo) ++ "<br />  " ++
    "预订权限: " ++ (toLevelString $ roomLevel roomInfo) ++ "<br />  " ++
    "即时启用: " ++ (boolToHanzi $ roomAvailable roomInfo) ++ "<br />  " ++
    "会议室有效期至: " ++ (show $ roomValidTime roomInfo) ++ "<br />  " ++
    "会议室添加日期: " ++ (show $ convertUtcToZoneTime $ roomFirstAdd roomInfo) ++ "<br />")