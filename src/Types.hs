{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Database.SQLite.Simple

type Port = Int

data Visit = Visit {ip :: T.Text, page :: T.Text, timestamp :: Int}
    deriving (Generic)

instance FromJSON Visit

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

newtype ErrMsg = ErrMsg {error :: String}
    deriving (Generic)

instance ToJSON ErrMsg

newtype Resp = Resp {msg :: String}
    deriving (Generic)

instance ToJSON Resp
