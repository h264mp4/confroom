{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.DayBookingStatus where

import Import
import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils
import qualified Data.HashMap.Strict as HM
import Data.Maybe(fromJust)
import Data.Aeson(toJSON, object, (.=))

-- 17 ge 
fields :: [Text]
fields = ["房间/时间","08","09","10","11","12","13","14","15","16",
                     "17","18","19","20","21","22","23","00"]
contents :: [[Text]]
contents = [ ["1001", "peng", "xing", "tao", "peng", "xing", "tao", "peng", "xing", "tao", 
                      "peng", "xing", "tao", "peng", "xing", "tao", "peng", "xing"      ],
             ["1002", "p2", "x2", "t2", "p2", "x2", "t2", "p2", "x2", "t2", 
                      "p2", "x2", "t2", "p2", "x2", "t2", "p2", "x2"      ],
             ["1003", "p3", "x3", "t3", "p3", "x3", "t3", "p3", "x3", "t3", 
                      "p3", "x3", "t3", "p3", "x3", "t3", "p3", "x3"      ],
             ["1004", "p4", "x4", "t4", "p4", "x4", "t4", "p4", "x4", "t4", 
                      "p4", "x4", "t4", "p4", "x4", "t4", "p4", "x4"      ]
           ]
              

-- for now, return fake data
fakeDataName = "dataRows"
--fakeDataRows :: [Value]
fakeDataRows = map (\ x -> object $ zipWith (\ k v -> (k, toJSON v)) fields x) contents

fakeJsonRet = object $ [fakeDataName .= fakeDataRows, "total" .= toJSON (4::Int)]

getDayBookingStatusR :: Handler Value -- actually return a json.
getDayBookingStatusR = do
    req <- waiRequest
    req' <- getRequest
    liftIO $ print req
    liftIO $ print $ reqLangs req'
    liftIO $ print fakeJsonRet    
    valueMB <- lookupGetParam "queryDay"
    case valueMB of
        Nothing -> liftIO $ print "not passed"
        Just x  -> liftIO $ print x
    return $ fakeJsonRet

