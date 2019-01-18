{-# LANGUAGE DeriveGeneric #-}
module Brockman.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Socket (HostName, PortNumber)

data NewsBot = NewsBot
  { b_nick :: Text
  , b_feeds :: [Text]
  , b_delay :: Int
  } deriving (Generic)

instance FromJSON NewsBot where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

data BrockmanConfig = BrockmanConfig
  { c_bots :: [NewsBot]
  , c_channels :: [Text]
  } deriving (Generic)

instance FromJSON BrockmanConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

data BrockmanOptions = BrockmanOptions
  { ircHost :: HostName
  , ircPort :: PortNumber
  , configFile :: FilePath
  , shortener :: Maybe String -- shortener URL
  }

