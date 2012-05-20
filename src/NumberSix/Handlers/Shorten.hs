{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Shorten
    ( handler
    ) where

import NumberSix.Irc
import NumberSix.Bang
import NumberSix.Util.BitLy

handler :: UninitializedHandler
handler = makeBangHandler "shorten" ["!shorten"] shorten