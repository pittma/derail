{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad (when)
import Control.Monad.IO.Class
import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Time
import Web.Scotty

data Visit = V {ip :: Text, page :: Text} deriving (Generic)

instance FromJSON Visit
instance ToJSON Visit

data LogLevel
    = Trace
    | Debug
    | Info
    | Error
    | Fatal
    deriving (Ord, Eq)

instance Show LogLevel where
    show Trace = "TRACE"
    show Debug = "DEBUG"
    show Info = "INFO"
    show Error = "ERROR"
    show Fatal = "FATAL"

newtype Logger = Logger LogLevel

logMsg :: (MonadIO m) => Logger -> LogLevel -> String -> m ()
logMsg (Logger level) msgLevel msg =
    when (msgLevel >= level) $ liftIO $ do
        ts <- getCurrentTime
        putStrLn (mkMsg ts)
  where
    mkMsg ts =
        " ["
            <> formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
            <> "] "
            <> show msgLevel
            <> ": "
            <> msg

logInfo :: (MonadIO m) => Logger -> String -> m ()
logInfo l = logMsg l Info

logError :: (MonadIO m) => Logger -> String -> m ()
logError l = logMsg l Error

handleVisit :: (MonadIO m) => Logger -> Visit -> m ()
handleVisit l visit = do
    logInfo l ("got IP [" <> unpack (ip visit) <> "]")
    logError l "time do to database stuff now but I have no database"

site :: Logger -> ScottyM ()
site l = do
    get "/alive" $ do
        text "hello"
    post "/visit" $ do
        v <- jsonData
        handleVisit l v
        text "OK"
