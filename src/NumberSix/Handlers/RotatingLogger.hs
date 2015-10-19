-- | Rotating logger for the PRIVMSG sent on the channels
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.RotatingLogger
    ( handler
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative     ((<$>))
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad.Reader    (ask)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Map                as Map
import           Data.Text               (Text, concat, pack, unpack)
import qualified Data.Text.IO            as T
import           Data.Time               (defaultTimeLocale)
import           Data.Time.Clock         (getCurrentTime)
import           Data.Time.Format        (formatTime)
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath.Posix   (joinPath)
import           System.IO               as IO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import           NumberSix.Irc
import           NumberSix.Message


--------------------------------------------------------------------------------
-- | A logfile is defined by a timestamp (YYYY-MM-DD) and a handle, that
-- represents the open file
type Log = (String, Handle)

--------------------------------------------------------------------------------
-- | There may be logs for multiple channels
type LogState = Map.Map Text Log


--------------------------------------------------------------------------------
-- | Handler for the logger
handler :: UninitializedHandler
handler = makeHandlerWith "RotatingLogger" [logHook] $ liftIO $ newMVar Map.empty

--------------------------------------------------------------------------------
-- | Saves the message to the appropriate file
logHook :: MVar LogState -> Irc ()
logHook mvar = onCommand "PRIVMSG" $ do

    channel <- getChannel
    sender <- toLower <$> getSender
    now <- liftIO getCurrentTime
    text <- getMessageText

    channelLogDir <- ircChannelLogDir . ircConfig . ircEnvironment <$> ask

    liftIO $ do
        let ymd = formatTime defaultTimeLocale "%Y-%m-%d" now
        let time = formatTime defaultTimeLocale "%c" now

        logState <- takeMVar mvar
        (logHandle, logState') <- getLog channelLogDir logState channel ymd

        T.hPutStrLn logHandle $ formatMessage (Data.Text.pack time) sender text
        IO.hFlush logHandle

        putMVar mvar logState'

    return ()

--------------------------------------------------------------------------------
-- | Format the log message
formatMessage :: Text  -- ^ timestamp
              -> Text  -- ^ sender nick
              -> Text  -- ^ message
              -> Text  -- ^ resulting log entry
formatMessage time sender message = Data.Text.concat ["[", time, "] : (", sender, ") : ", message]

--------------------------------------------------------------------------------
-- | Get the handle to the log file for the (channel, date) combo or
-- open a new log file for that
getLog :: String                -- ^ Base directory for storing channel logs
       -> LogState              -- ^ Current map between channels and (log name, handle)
       -> Text                  -- ^ Channel name
       -> String                -- ^ Date string, i.e., log name
       -> IO (Handle, LogState) -- ^ New mapping
getLog channelLogBaseDir state channel ymd =
    case Map.lookup channel state of
        Just (currentYmd, currentHandle)
            | currentYmd == ymd -> return (currentHandle, state)
            | otherwise         -> do IO.hClose currentHandle
                                      newLog
        Nothing -> newLog
  where newLog = do
            let logDirectory = joinPath [channelLogBaseDir, Data.Text.unpack channel]
            createDirectoryIfMissing True logDirectory
            handle <- IO.openFile (joinPath [logDirectory, ymd]) IO.AppendMode
            let state' = Map.alter (\_ -> Just (ymd, handle)) channel state
            return (handle, state')
