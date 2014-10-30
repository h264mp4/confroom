{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Manage where

import Import
import CommonWidget
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils

------------------------------------------------------------------------------------------
---- Admin Manage Page
getManageR :: Handler Html
getManageR = defaultLayout $ do
    let link = AddRoomR 
        dataType = ("typeroom"::Text) 
        buttonName = ("新建会议室":: Text) 
    aRandonId <- newIdent
    aRandomTableId <- newIdent
    toWidget $(widgetFile "manage")

getManageUserR :: Handler Html
getManageUserR = defaultLayout $ do
    let link = ListUserR 
        dataType = ("typeuser"::Text) 
        buttonName = ("新建用户":: Text) 
    aRandonId <- newIdent
    aRandomTableId <- newIdent
    $(widgetFile "manage")

getManageRoomR :: Handler Html
getManageRoomR = defaultLayout $ do
    let link = ListRoomR 
        dataType = ("typeroom"::Text) 
        buttonName = ("新建会议室":: Text) 
    aRandonId <- newIdent
    aRandomTableId <- newIdent
    $(widgetFile "manage")
