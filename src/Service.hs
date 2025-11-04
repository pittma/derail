{-# LANGUAGE OverloadedStrings #-}

module Service (service, LogLevel (..), Config (..)) where

import Control.Exception.Safe (catchAny, displayException)
import Control.Monad.IO.Class

import Crypto.Hash
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as BS
import Data.Byteable
import qualified Data.Text as T
import Data.Text.Encoding
import Database.SQLite.Simple
import Network.HTTP.Types.Status
import Web.Scotty

import Logger
import Types

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
            `catchAny` \e ->
                logError
                    l
                    ( "inserting visit to ["
                        <> T.unpack (page visit)
                        <> "] with timestamp ["
                        <> show (timestamp visit)
                        <> "] failed: "
                        <> displayException e
                    )

dbInit :: IO Connection
dbInit = do
    conn <- open "derail.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS visits (id INTEGER PRIMARY KEY, iphash BLOB, page TEXT, timestamp INTEGER)"
    pure conn

service :: Config -> IO ()
service cfg =
    let l = Logger (llevel cfg)
     in dbInit >>= \c -> scotty (port cfg) $ do
            get "/alive" $ do
                text "hello"
            post "/visit" $ do
                b <- body
                case eitherDecode b of
                    Left e -> do
                        status badRequest400
                        logError l ("JSON parsing failed: " <> e)
                        json (ErrMsg e)
                    Right v -> do
                        handleVisit cfg l c v
                        json (Resp "success")
