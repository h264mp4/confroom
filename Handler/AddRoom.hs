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

sampleForm :: Form Room
sampleForm = renderDivs $ Room
    <$> areq textField "会议室编号" Nothing
    <*> areq (selectFieldList authLevel) "预订权限" Nothing
    <*> areq 

    where
    authLevel :: [(Text, Level)]
    authLevel = [("普通", AuthNormal), ("领导", AuthAdvance),("管理员", AuthAdmin)]

    lift (liftIO getCurrentTime)

Room
    number  Text
    available Bool
    firstAdd UTCTime
    validTime UTCTime
    level Level
    UniqueRoomNo number
    deriving Show


data Car = Car
    { carModel :: Text
    , carYear :: Int
    , carColor :: Maybe Color
    }
  deriving Show

data Color = Red | Blue | Gray | Black
    deriving (Show, Eq, Enum, Bounded)

carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq carYearField "Year" (carYear <$> mcar)
    <*> aopt (selectFieldList colors) "Color" (carColor <$> mcar)
  where
    colors :: [(Text, Color)]
    colors = [("Red", Red), ("Blue", Blue), ("Gray", Gray), ("Black", Black)]
