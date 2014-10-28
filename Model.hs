{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Data.ByteString (ByteString)
import Data.Time
import Handler.MiscTypes
import Data.Aeson(ToJSON(..), object, (.=))

import Handler.Utils

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
    toJSON (Entity key userInfo) = object $ 
                               [ "Email" .= (show $ userEmail userInfo)
                               , "姓名"  .= (show $ userName userInfo) 
                               , "权限"  .= (toLevelString $ userLevel userInfo) 
                               , "注册时间" .= (show $ convertUtcToZoneTime $ userFirstAdd userInfo)
                               , "sqlkey" .= (show $ fromSqlKey key)
                               ]

instance ToJSON (Entity Room) where
    toJSON (Entity key roomInfo) = object $ 
                               [ "会议室编号" .= (show $ roomNumber roomInfo)
                               , "权限" .= (toLevelString $ roomLevel roomInfo)
                               , "启用" .= ((boolToHanzi $ roomAvailable roomInfo) :: String)
                               , "有效期"  .= (show $ roomValidTime roomInfo)
                               , "注册时间" .= (show $ convertUtcToZoneTime $ roomFirstAdd roomInfo)
                               , "sqlkey" .= (show $ fromSqlKey key)
                               ]

------------------------------------------------------------------------------------------
---- type helper functions
toLevelString :: Level -> String
toLevelString lev
    | lev == AuthNormal  = "普通 "
    | lev == AuthAdvance = "领导"
    | lev == AuthAdmin   = "管理员"
    | otherwise          = "Wrong Level"

authLevel :: [(Text, Level)]
authLevel = [("普通", AuthNormal), ("领导", AuthAdvance),("管理员", AuthAdmin)]
