{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.User where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(fromJust)
import Data.Aeson(object, (.=))
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 

getAddUserR :: Handler Html
getAddUserR = do
    (addUserWidget, formEnctype) <- generateFormPost addUserForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getAddUserR" :: Text

    defaultLayout $ do
        addUserFormId <- newIdent
        $(widgetFile "adduser")

postAddUserR :: Handler Html
postAddUserR = do
    ((result, formWidget), formEnctype) <- runFormPost addUserForm
    let handlerName = "postAddUserR" :: Text
    case result of
        FormFailure errMsg -> defaultLayout $ backWidget ("无效的会议室信息, 请重新输入." :: String)

        FormSuccess formInfo -> do
            mayUserId <- runDB $ addNewUser formInfo
            case mayUserId of
                 Nothing -> defaultLayout $ backWidget  ("会议室信息已存在，请重新输入" :: String)

                 Just userId -> do
                     liftIO $ print ("Add new user done: " ++ show (fromJust mayUserId))
                     liftIO $ print formInfo
                     defaultLayout $ backWidget (toHtmlUserInfo formInfo)

    where
    backWidget info = toWidget [hamlet|
                            <div class="row">
                                <div class="col-md-12">
                                    <input type=button value="返回" class="btn btn-primary" onClick="location.href='@{AddUserR}'">
                                <div class="col-md-12">
                                     <h3> 会议室信息已保存
                                <div class="col-md-12"> 
                                    <p> #{preEscapedToMarkup info}                     
|]

simpleFormLayoutForAddUser = BootstrapHorizontalForm
                             {
                                  bflLabelOffset = ColMd 0
                                 ,bflLabelSize   = ColMd 4
                                 ,bflInputOffset = ColMd 0
                                 ,bflInputSize   = ColMd 4
                             }

addUserForm :: Form User
addUserForm = renderBootstrap3 simpleFormLayoutForAddUser $ User
        <$> areq emailField "电子邮箱" Nothing
        <*> areq textField "密码" (Just "physics")
        <*> areq textField "姓名" Nothing
        <*> areq (selectFieldList authLevel) "权限" Nothing
        <*> pure "" -- areq textField "resetKey" (Just "physics")
        <*> lift (liftIO getCurrentTime)

getListUserR :: Handler Value
getListUserR = do
    -- TODO: User Auth Widget
    users <- runDB $ listUserProfile
    if null users
       then return $ object $ []
       else do
            return $ object $ ["dataRows" .= (map toJSON users), 
                               "total" .= toJSON (length users :: Int)
                              ]

------------------------------------------------------------------------------------------
---- other helpers
toHtmlUserInfo :: User -> String
toHtmlUserInfo userInfo = ( 
    "姓名: " ++ (show $ userName userInfo) ++ "<br />  " ++
    "权限: " ++ (show $ userLevel userInfo) ++ "<br />  " ++
    "电子邮箱: " ++ (show $ userEmail userInfo) ++ "<br />  ")