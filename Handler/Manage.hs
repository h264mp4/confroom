{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Manage where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils

------------------------------------------------------------------------------------------
---- Admin Manage Page

getManageR :: Handler Html
getManageR = do
    defaultLayout $ do
        $(widgetFile "manage")

