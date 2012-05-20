{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Twitter
    ( handler
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Char (isDigit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Aeson
import qualified Data.Vector as V

import NumberSix.Irc
import NumberSix.Message
import NumberSix.Bang
import NumberSix.Util
import NumberSix.Util.Http

data User = User ByteString

instance FromJSON User where
    parseJSON (Object o) = User <$> o .: "screen_name"
    parseJSON _ = mzero

data Tweet = Tweet User ByteString

instance FromJSON Tweet where
    parseJSON (Object o) = Tweet <$> o .: "user" <*> o .: "text"
    parseJSON (Array a) = if V.null a then mzero else parseJSON (V.head a)
    parseJSON _ = mzero

getTweet :: ByteString -> ByteString
getTweet bs = case parseJsonEither bs of
    Left _ -> "Tweet not found"
    Right (Tweet (User user) text) ->
        if "RT " `B.isPrefixOf` text then text else "@" <> user <> ": " <> text

twitter :: ByteString -> Irc ByteString
twitter argument
    | B.all isDigit argument = getTweet <$> httpGet (tweet argument)
    | B.all isDigit fromUrl  = getTweet <$> httpGet (tweet fromUrl)
    | otherwise              = getTweet <$> httpGet (user argument)
  where
    fromUrl = B.reverse $ B.takeWhile (/= '/') $ B.reverse argument

    user u = "http://api.twitter.com/1/statuses/user_timeline.json?screen_name="
           <> urlEncode u <> "&include_rts=1&count=1"
    tweet t =  "http://api.twitter.com/1/statuses/show/"
            <> urlEncode t <> ".json"

handler :: UninitializedHandler
handler = makeBangHandler "twitter" ["!twitter"] twitter