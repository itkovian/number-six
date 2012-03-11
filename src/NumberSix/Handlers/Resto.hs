{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Resto
    ( handler
    ) where

import Control.Monad (mzero)
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Time (UTCTime (..), addDays, formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M

import Data.Aeson (FromJSON (..), Value (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Http

data WeekMenu = WeekMenu (M.Map String [String]) deriving (Show)

instance FromJSON WeekMenu where
    parseJSON (Object o) = return $ WeekMenu $ M.fromListWith (++)
        [ (T.unpack day, [T.unpack name])
        | (day, Object menu) <- HM.toList o
        , Array meats        <- maybeToList $ HM.lookup "meat" menu
        , Object meat        <- V.toList meats
        , String name        <- maybeToList $ HM.lookup "name" meat
        ]
    parseJSON _          = mzero

resto :: ByteString -> Irc ByteString
resto arg = do
    currentTime <- liftIO $ getCurrentTime
    let time = currentTime {utctDay = days arg `addDays` utctDay currentTime}
        week = formatTime defaultTimeLocale "%U"       time
        day  = formatTime defaultTimeLocale "%Y-%m-%d" time
        url  = "http://kelder.zeus.ugent.be/~blackskad/resto/api/0.1/week/" ++
            dropWhile (== '0') week ++ ".json"
   
    httpGet (BC.pack url) >>= \bs -> return $ case parseJsonEither bs of
        Left _             -> "Please throw some rotten tomatoes at blackskad."
        Right (WeekMenu m) -> case M.lookup day m of
            Nothing -> "Resto's not open today..."
            Just ms -> T.encodeUtf8 $ T.pack $ intercalate ", " ms
  where
    days "tomorrow"           = 1
    days "morgen"             = 1
    days "day after tomorrow" = 2
    days "overmorgen"         = 2
    days _                    = 0

handler :: Handler
handler = makeBangHandler "resto" ["!resto"] resto
