{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad (when)
import Control.Monad.IO.Class
import GHC.Generics

import Crypto.Hash
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.Byteable
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Database.SQLite.Simple
import Web.Scotty

type Port = Int

data Visit = Visit {ip :: T.Text, page :: T.Text, timestamp :: Int}
    deriving (Generic)

instance FromJSON Visit
instance ToJSON Visit

data VisitRow = VisitRow {iphash :: BS.ByteString, rowpage :: T.Text, rowts :: Int}

instance ToRow VisitRow where
    toRow (VisitRow iph p ts) = toRow (iph, p, ts)

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

data Config = Cfg {salt :: String, port :: Port, llevel :: LogLevel}

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

logInfo :: (MonadIO m) => Logger -> String -> m ()
logInfo l = logMsg l Info

logError :: (MonadIO m) => Logger -> String -> m ()
logError l = logMsg l Error

hashIP :: String -> T.Text -> BS.ByteString
hashIP slt ips =
    let input = encodeUtf8 (ips <> T.pack slt)
     in BS.take 16 $ toBytes (hash input :: Digest SHA256)

handleVisit :: (MonadIO m) => Config -> Logger -> Connection -> Visit -> m ()
handleVisit cfg l conn visit = do
    liftIO $
        execute
            conn
            "INSERT INTO visits (iphash, page, timestamp) VALUES (?,?,?)"
            (VisitRow (hashIP (salt cfg) (ip visit)) (page visit) (timestamp visit))

dbInit :: IO Connection
dbInit = do
    conn <- open "derail.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS visits (id INTEGER PRIMARY KEY, iphash BLOB, page TEXT, timestamp INTEGER)"
    pure conn

site :: Config -> Logger -> IO ()
site cfg l =
    dbInit >>= \c -> scotty (port cfg) $ do
        get "/alive" $ do
            text "hello"
        post "/visit" $ do
            v <- jsonData
            handleVisit cfg l c v
            text "OK"
