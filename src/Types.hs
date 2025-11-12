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
    Route (..),
    App,
) where

import GHC.Generics

import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Database.SQLite.Simple

type Port = Int

data Visit = Visit
    { pageRoute :: T.Text
    , pageTitle :: T.Text
    , pageReferrer :: T.Text
    }
    deriving (Show, Generic)

instance FromJSON Visit

data VisitRow = VisitRow
    { iphash :: BS.ByteString
    , rowpath :: T.Text
    , rowtitle :: T.Text
    , rowts :: Int
    , rowreferrer :: T.Text
    }

instance ToRow VisitRow where
    toRow (VisitRow iph path t ts rfr) = toRow (iph, path, t, ts, rfr)

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
    , token :: BS8.ByteString
    }
    deriving (Generic)

data Err
    = JSON String
    | Db (Maybe Visit) String
    | Req String
    deriving (Show)

newtype ErrMsg = ErrMsg {error :: String}
    deriving (Generic)

instance ToJSON ErrMsg

newtype Resp = Resp {msg :: String}
    deriving (Generic)

instance ToJSON Resp

data Route = Route
    { route :: T.Text
    , title :: T.Text
    , visits :: Int
    }
    deriving (Show, Generic)

instance ToJSON Route

instance FromRow Route

type App m a = ExceptT Err m a
