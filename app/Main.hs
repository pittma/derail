{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import GHC.Generics
import System.IO (BufferMode (..), hSetBuffering, stdout)

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml (decodeFileThrow)
import Options.Applicative (Parser, argument, execParser, fullDesc, helper, info, metavar, str, (<**>))
import System.Directory

import Service (Config (..), LogLevel (..), Mode (..), service)

newtype Derail = Derail String

data Conf = Conf
    { mode :: String
    , salt :: String
    , port :: Int
    , log_level :: String
    , origin :: String
    , db_path :: String
    }
    deriving (Generic)

instance FromJSON Conf

toLogLevel :: String -> IO LogLevel
toLogLevel s = case s of
    "TRACE" -> pure Trace
    "DEBUG" -> pure Debug
    "INFO" -> pure Info
    "ERROR" -> pure Error
    "FATAL" -> pure Fatal
    _ -> throwIO $ userError (s <> " is an invalid log level")

toMode :: String -> IO Mode
toMode s = case s of
    "proxied" -> pure Proxied
    "direct" -> pure Direct
    _ -> throwIO $ userError (s <> " is not a valid mode")

optParser :: Parser Derail
optParser = Derail <$> argument str (metavar "CONFIG-PATH")

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    out <-
        ( try $ do
            (Derail path) <- execParser (info (optParser <**> helper) fullDesc)
            cp <- makeAbsolute path
            (Conf m s p ll o dbp) <- decodeFileThrow cp
            dbp' <- makeAbsolute dbp
            level <- toLogLevel ll
            md <- toMode m
            pure $ Cfg md s p level (BS8.pack o) dbp'
        ) ::
            IO (Either IOError Config)
    case out of
        Right cfg -> service cfg
        Left e -> putStrLn ("starting the service failed: " <> show e)
