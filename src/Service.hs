{-# LANGUAGE OverloadedStrings #-}

module Service where

import Control.Monad.IO.Class

import Crypto.Hash
import qualified Data.ByteString as BS
import Data.Byteable
import qualified Data.Text as T
import Data.Text.Encoding
import Database.SQLite.Simple
import Web.Scotty

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

dbInit :: IO Connection
dbInit = do
    conn <- open "derail.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS visits (id INTEGER PRIMARY KEY, iphash BLOB, page TEXT, timestamp INTEGER)"
    pure conn

service :: Config -> Logger -> IO ()
service cfg l =
    dbInit >>= \c -> scotty (port cfg) $ do
        get "/alive" $ do
            text "hello"
        post "/visit" $ do
            v <- jsonData
            handleVisit cfg l c v
            text "OK"
