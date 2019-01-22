{-# LANGUAGE DeriveGeneric #-}

module Brockman.Types where

import Data.Aeson
import Data.Char (toLower, isLower)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Socket (HostName, PortNumber)
import Data.Default (Default(def))

data NewsBot = NewsBot
  { botNick :: Text
  , botFeeds :: [Text]
  , botDelay :: Int
  } deriving (Generic)

instance FromJSON NewsBot where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = map toLower . dropWhile isLower}

data BotsConfig = BotsConfig
  { configBots :: [NewsBot]
  , configChannels :: [Text]
  } deriving (Generic)

instance FromJSON BotsConfig where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = map toLower . dropWhile isLower}

instance Default BotsConfig where
  def = BotsConfig [] []

data BrockmanOptions = BrockmanOptions
  { ircHost :: HostName
  , ircPort :: PortNumber
  , configFile :: FilePath
  , shortener :: Maybe HostName
  , useTLS :: Bool
  }
