module Logger (logInfo, logDebug, logError) where

import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Time

import Types (LogLevel (..), Logger (..))

logMsg :: (MonadIO m) => Logger -> LogLevel -> String -> m ()
logMsg (Logger level) msgLevel msg =
    when (msgLevel >= level) $ liftIO $ do
        ts <- getZonedTime
        putStrLn (mkMsg ts)
  where
    mkMsg ts =
        " ["
            <> formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
            <> "] "
            <> show msgLevel
            <> ": "
            <> msg

logDebug :: (MonadIO m) => Logger -> String -> m ()
logDebug l = logMsg l Debug

logInfo :: (MonadIO m) => Logger -> String -> m ()
logInfo l = logMsg l Info

logError :: (MonadIO m) => Logger -> String -> m ()
logError l = logMsg l Error
