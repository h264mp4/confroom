{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.AddRoom where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 

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
        maybeFormInfo = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    mayRoomId <- runDB $ addNewRoom (fromJust maybeFormInfo)
    liftIO $ print ("Add new room done: " ++ show (fromJust mayRoomId))
    liftIO $ print maybeFormInfo
    defaultLayout $ do
        toWidget 
          [hamlet|
            <div class="row">
                <div class="col-md-12">
                    <h3> 会议室信息已保存
                <div class="col-md-12"> 
                    $maybe formInfo  <- maybeFormInfo
                        <p> #{show formInfo} 
                    $nothing
                        <p> 无效的会议室信息，请从新输入
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
        <*> areq boolField "是否现在启用" Nothing
        <*> areq (jqueryDayField def {jdsChangeMonth = True, jdsChangeYear = True}) 
                                 "会议室有效期至" Nothing
        <*> lift (liftIO getCurrentTime)

    where
    authLevel :: [(Text, Level)]
    authLevel = [("普通", AuthNormal), ("领导", AuthAdvance),("管理员", AuthAdmin)]
