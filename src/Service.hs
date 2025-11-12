{-# LANGUAGE OverloadedStrings #-}

module Service (service, LogLevel (..), Config (..), Mode (..)) where

import Control.Monad.IO.Class

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

import qualified App as A
import Logger
import Types

hashIP :: String -> String -> BS.ByteString
hashIP slt ips =
    let input = BS8.pack (ips <> slt)
     in BS.take 16 $ toBytes (hash input :: Digest SHA256)

decodeVisit :: (MonadIO m) => BSL.ByteString -> App m Visit
decodeVisit b = A.fromEitherWith JSON (eitherDecode b)

insertVisit :: (MonadIO m) => Connection -> BS.ByteString -> Visit -> App m ()
insertVisit conn ipHash v@(Visit rt page rfr) = do
    ts <- liftIO getPOSIXTime
    A.executeWithVisit
        v
        conn
        "INSERT INTO visits (iphash, route, title, timestamp, referrer) VALUES (?,?,?,?,?)"
        (VisitRow ipHash rt page (floor ts) rfr)

getTopRoutes :: (MonadIO m) => Connection -> App m [Route]
getTopRoutes c = A.query_ c "SELECT route, title, COUNT(*) AS count FROM visits GROUP BY route ORDER BY count DESC"

dbInit :: Logger -> FilePath -> IO Connection
dbInit l dbp = do
    logInfo l ("opening database at " <> dbp)
    conn <- open dbp
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

getIp :: (Applicative m) => Request -> Mode -> App m String
getIp r m = A.fromEither $ case m of
    Proxied -> case lookup "X-Forwarded-For" (requestHeaders r) of
        Just ff -> Right (BS8.unpack ff)
        Nothing -> Left $ Req "missing X-Forwarded-For header"
    Direct -> Right (show $ remoteHost r)

checkToken :: (Monad m) => BS8.ByteString -> Maybe BS8.ByteString -> App m ()
checkToken t h = A.fromEither $ case h of
    Just hd
        | t == hd -> Right ()
        | otherwise -> Left (Req "invalid access token")
    Nothing -> Left (Req "access token missing")

service :: Config -> IO ()
service cfg =
    let l = Logger (llevel cfg)
     in do
            c <- dbInit l (db cfg)
            app <- scottyApp $ do
                middleware (corsMiddleware [origin cfg])
                middleware (referrerGuard (origin cfg) l)
                get "/alive" $ do
                    json (Resp "alive")
                get "/top" $ do
                    req <- request
                    res <-
                        A.run $ do
                            checkToken (token cfg) (lookup "X-access-token" (requestHeaders req))
                            getTopRoutes c

                    case res of
                        Left (Db _ e) -> do
                            logError l ("query failed: " <> e)
                            json (ErrMsg e)
                        Left (Req m) -> do
                            logError l ("invalid request to /top: " <> m)
                            status forbidden403
                            json (ErrMsg m)
                        Left _ -> do
                            logError l "unknown error occurred when retrieving top routes"
                            status internalServerError500
                            json (ErrMsg "unknwon error occurred")
                        Right r -> do
                            json r
                options "/visit" $ do
                    logDebug l "OPTION hit"
                    req <- request
                    logDebug l (show $ requestHeaders req)
                    text "ok"
                post "/visit" $ do
                    b <- body
                    req <- request
                    res <- A.run $ do
                        ip <- getIp req (mode cfg)
                        v <- decodeVisit b
                        insertVisit c (hashIP (salt cfg) ip) v
                    case res of
                        Left (JSON m) -> do
                            logError l ("decoding visit failed: " <> m)
                            status badRequest400
                            json (ErrMsg m)
                        Left (Req m) -> do
                            logError l ("invalid request: " <> m)
                            status badRequest400
                            json (ErrMsg m)
                        Left (Db (Just v) m) -> do
                            logError l ("insertion of visit to " <> T.unpack (pageRoute v) <> "failed: " <> m)
                            status internalServerError500
                            json (ErrMsg m)
                        Left _ -> do
                            logError l "unknown error occurred on visit insertion"
                            status internalServerError500
                            json (ErrMsg "unknown error")
                        Right _ -> logDebug l "visit saved" >> json (Resp "success")
            logInfo l "ready."
            run (port cfg) app
