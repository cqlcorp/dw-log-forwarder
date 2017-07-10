{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings,
             ScopedTypeVariables #-}

module Config where

import qualified Data.ByteString.Lazy as BS

import           Data.Aeson
import           GHC.Generics

data StreamOutput = StreamOutput
  { outputFolder :: Maybe String
  , logstashUrl  :: Maybe String
  } deriving (Generic, Show, FromJSON)

data DiagnosticsInfo = DiagnosticsInfo
  { diagnosticsEmails :: [String]
  , fromEmail         :: String
  , smtpServer        :: String
  } deriving (Generic, Show, FromJSON)

data Config = Config
  { logShippersConnString     :: String
  , encPasswordPassphrase     :: String
  , output                    :: StreamOutput
  , archiveQueueDir           :: String
  , maxDayQueueProcessSeconds :: Int
  , appLogFolder              :: String
  , diagnostics               :: DiagnosticsInfo
  }
  deriving (Generic, Show, FromJSON)

loadConfig :: String -> IO Config
loadConfig file = do
  contents <- BS.readFile file
  let config = decode contents
  case config of
    Nothing -> error $ "Could not parse config file " ++ file
    Just c  -> return c
