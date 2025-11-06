{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Options.Applicative

import Service

data Derail = Derail String Int String LogLevel

llParser :: ReadM LogLevel
llParser = eitherReader $ \s -> case s of
    "TRACE" -> Right Trace
    "DEBUG" -> Right Debug
    "INFO" -> Right Info
    "ERROR" -> Right Error
    "FATAL" -> Right Fatal
    _ -> Left (s <> " is not a valid log level")

optParser :: Parser Derail
optParser =
    Derail
        <$> argument str (metavar "SALT")
        <*> argument auto (metavar "PORT")
        <*> argument str (metavar "ORIGIN")
        <*> argument llParser (metavar "LOG-LEVEL")

main :: IO ()
main = do
    (Derail s p o ll) <- execParser (info (optParser <**> helper) fullDesc)
    service (Cfg s p ll (BS.pack o))
