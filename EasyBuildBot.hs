-- | Number six sample configuration file
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import NumberSix
import NumberSix.Irc

main :: IO ()
main = numberSix IrcConfig
    { ircNick        = "easybuilder2"
    , ircRealName    = "Number Six Testing Bot"
    , ircChannels    = ["#easybuild"]
    , ircHost        = "chat.freenode.net"
    , ircPort        = 6667
    , ircGodPassword = "foobar"
    , ircDatabase    = "dbname = number-six"
    , ircNickServ    = Nothing
    }
