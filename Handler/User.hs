{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.User where

import Import
import CommonWidget
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import Data.Maybe(isJust, fromJust)
import Data.Aeson(object, (.=))
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3 
import Database.Persist.Sql(toSqlKey)

import Data.Int(Int64)
import Data.Text(unpack)
import Data.Conduit
import Data.Text.Encoding(encodeUtf8)
import Data.ByteString.Lazy(fromStrict)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import Data.Aeson(ToJSON(..), object, (.=), decode)


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
        FormFailure errMsg -> defaultLayout $ do
                 backNavWidget emptyText ("无效的用户信息, 请重新输入." :: Text) ManageUserR

        FormSuccess formInfo -> do
            mayUserId <- runDB $ addNewUser formInfo
            case mayUserId of
                 Nothing -> defaultLayout $ do
                      backNavWidget emptyText ("用户信息已存在，请重新输入" :: Text) ManageUserR

                 Just userId -> do
                     liftIO $ print ("Add new user done: " ++ show (fromJust mayUserId))
                     liftIO $ print formInfo
                     defaultLayout $ do
                         backNavWidget ("用户信息已保存"::Text) (toHtmlUserInfo formInfo) ManageUserR

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

getEditUserR :: Handler Html
getEditUserR = defaultLayout $ do
    toWidget [hamlet| <p> it is ok user |]

deleteDeleteUserR :: Handler Value
deleteDeleteUserR = do
    texts <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
    liftIO $ print texts
    let mayId = decode . fromStrict . encodeUtf8 $ texts !! 0
        bValidData = isJust mayId
    if bValidData
       then do
            doDelete $ fromJust mayId
            return $ object $ [("ret" :: Text) .= ("ok" :: Text)]
       else return $ object $ [("ret" :: Text) .= ("invalid data" :: Text)]

    where 
    doDelete deleteObj = do
        let theId = toSqlKey $ (read . unpack $ deleteId deleteObj)
        liftIO $ print theId
        runDB $ deleteUser (theId :: Key User)
        return ()


------------------------------------------------------------------------------------------
---- other helpers
toHtmlUserInfo :: User -> Text
toHtmlUserInfo userInfo = ( 
    "姓名: " <> (userName userInfo) <> "<br />  " <>
    "权限: " <> (toLevelString $ userLevel userInfo) <> "<br />  " <>
    "电子邮箱: " <> (userEmail userInfo) <> "<br />  ")