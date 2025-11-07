{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (
    Port,
    Visit (..),
    VisitRow (..),
    LogLevel (..),
    Logger (..),
    Config (..),
    Err (..),
    ErrMsg (..),
    Resp (..),
    Mode (..),
) where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Database.SQLite.Simple

type Port = Int

data Visit = Visit
    { pageRoute :: T.Text
    , pageTitle :: T.Text
    }
    deriving (Generic)

instance FromJSON Visit

data VisitRow = VisitRow
    { iphash :: BS.ByteString
    , rowpath :: T.Text
    , rowtitle :: T.Text
    , rowts :: Int
    }

instance ToRow VisitRow where
    toRow (VisitRow iph path title ts) = toRow (iph, path, title, ts)

data LogLevel
    = Trace
    | Debug
    | Info
    | Error
    | Fatal
    deriving (Ord, Eq, Generic)

instance Show LogLevel where
    show Trace = "TRACE"
    show Debug = "DEBUG"
    show Info = "INFO"
    show Error = "ERROR"
    show Fatal = "FATAL"

newtype Logger = Logger LogLevel

data Mode = Proxied | Direct

data Config = Cfg
    { mode :: Mode
    , salt :: String
    , port :: Port
    , llevel :: LogLevel
    , origin :: BS.ByteString
    , db :: FilePath
    }
    deriving (Generic)

data Err
    = JSON String
    | DB Visit String
    | Req String

newtype ErrMsg = ErrMsg {error :: String}
    deriving (Generic)

instance ToJSON ErrMsg

newtype Resp = Resp {msg :: String}
    deriving (Generic)

instance ToJSON Resp
