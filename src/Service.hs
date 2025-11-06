{-# LANGUAGE OverloadedStrings #-}

module Service (service, LogLevel (..), Config (..)) where

import Control.Exception (try)
import Control.Monad.IO.Class
import Data.Bifunctor

import Control.Monad.Trans.Except
import Crypto.Hash
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Byteable
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Web.Scotty

import Logger
import Types

hashIP :: String -> String -> BS.ByteString
hashIP slt ips =
    let input = BS8.pack (ips <> slt)
     in BS.take 16 $ toBytes (hash input :: Digest SHA256)

decodeVisit :: (MonadIO m) => BSL.ByteString -> ExceptT Err m Visit
decodeVisit b = ExceptT $ pure $ first JSON (eitherDecode b)

insertVisit :: (MonadIO m) => Connection -> BS.ByteString -> Visit -> ExceptT Err m ()
insertVisit conn ipHash v@(Visit route page) =
    ExceptT $
        liftIO $ do
            res <-
                try $ do
                    ts <- getPOSIXTime
                    execute
                        conn
                        "INSERT INTO visits (iphash, route, title, timestamp) VALUES (?,?,?,?)"
                        (VisitRow ipHash route page (floor ts))
            case res of
                Left (SQLError _ m _) -> pure $ Left (DB v (T.unpack m))
                Right _ -> pure $ Right ()

dbInit :: Logger -> IO Connection
dbInit l = do
    logInfo l "opening database at derail.db"
    conn <- open "derail.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS visits (id INTEGER PRIMARY KEY, iphash BLOB, route TEXT, title TEXT, timestamp INTEGER)"
    pure conn

corsMiddleware :: [Origin] -> Middleware
corsMiddleware origins =
    let policy =
            simpleCorsResourcePolicy
                { corsOrigins = Just (origins, True)
                , corsRequestHeaders = ["Content-Type"]
                }
     in cors (const $ Just policy)

referrerGuard :: BS.ByteString -> Logger -> Middleware
referrerGuard rfr l app req resp =
    case lookup "Referer" (requestHeaders req) of
        Just ref
            | rfr `BS.isPrefixOf` ref -> app req resp
            | otherwise -> forbidden ("referer " <> BS8.unpack ref <> " is invalid")
        Nothing -> forbidden "missing referer header"
  where
    forbidden m = do
        logError l m
        resp
            ( responseLBS
                forbidden403
                [("Content-Type", "text/json")]
                "{\"error\": \"forbidden\"}"
            )

service :: Config -> IO ()
service cfg =
    let l = Logger (llevel cfg)
     in do
            c <- dbInit l
            app <- scottyApp $ do
                middleware (corsMiddleware [origin cfg])
                middleware (referrerGuard (origin cfg) l)
                get "/alive" $ do
                    text "hello"
                options "/visit" $ do
                    logDebug l "OPTION hit"
                    req <- request
                    logDebug l (show $ requestHeaders req)
                    text "ok"
                post "/visit" $ do
                    b <- body
                    req <- request
                    let ip = show $ remoteHost req
                    res <- runExceptT $ do
                        v <- decodeVisit b
                        insertVisit c (hashIP (salt cfg) ip) v
                    case res of
                        Left (JSON m) -> do
                            logError l ("decoding visit failed: " <> m)
                            status badRequest400
                            json (ErrMsg m)
                        Left (DB v m) -> do
                            logError l ("insertion of visit to " <> T.unpack (pageRoute v) <> "failed: " <> m)
                            status internalServerError500
                            json (ErrMsg m)
                        Right _ -> logDebug l "visit saved" >> json (Resp "success")
            logInfo l "ready."
            run (port cfg) app
