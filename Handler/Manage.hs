{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Manage where

import Import
import CommonWidget
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils

------------------------------------------------------------------------------------------
---- Admin Manage Page

getManageR = getManageRoomR
getManageRoomR = getManagePage AddRoomR ("typeroom"::Text) ("新建会议室":: Text) 
getManageUserR = getManagePage AddUserR ("typeuser"::Text) ("新建用户"  :: Text) 

getManagePage link dataType buttonName = defaultLayout $ do
    aRandomId <- newIdent
    aRandomTableId <- newIdent
    $(widgetFile "manage")