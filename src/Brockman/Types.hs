{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase #-}

module Brockman.Types
  ( BrockmanConfig(..)
  , BotConfig(..)
  , ControllerConfig(..)
  , IRCConfig(..)
  ) where

import Data.Aeson
import Data.Char (isLower, toLower)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data BrockmanConfig = BrockmanConfig
  { configBots :: Map Text BotConfig
  , configUseTls :: Maybe Bool
  , configIrc :: IRCConfig
  , configShortener :: Maybe Text
  , configController :: Maybe ControllerConfig
  } deriving (Generic, Show)

data ControllerConfig = ControllerConfig
  { controllerNick :: Text
  , controllerChannels :: [Text]
  } deriving (Generic, Show)

data IRCConfig = IrcConfig
  { ircHost :: Text
  , ircPort :: Maybe Int
  } deriving (Generic, Show)

data BotConfig = BotConfig
  { botFeed :: Text
  , botChannels :: [Text]
  , botDelay :: Maybe Int
  } deriving (Generic, Show)

myOptions :: Options
myOptions =
  defaultOptions {fieldLabelModifier = uncapitalize . dropWhile isLower}
  where
    uncapitalize =
      \case
        [] -> []
        (x:xs) -> toLower x : xs

instance FromJSON BrockmanConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON BotConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON IRCConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON ControllerConfig where
  parseJSON = genericParseJSON myOptions

instance ToJSON BrockmanConfig where
  toJSON = genericToJSON myOptions

instance ToJSON BotConfig where
  toJSON = genericToJSON myOptions

instance ToJSON IRCConfig where
  toJSON = genericToJSON myOptions

instance ToJSON ControllerConfig where
  toJSON = genericToJSON myOptions
