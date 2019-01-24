{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase #-}

module Brockman.Types
  ( BrockmanConfig(..)
  , BotConfig(..)
  , ControllerConfig(..)
  , ShortenerConfig(..)
  , IRCConfig(..)
  , cBots
  , bFeed
  , bDelay
  , bChannels
  ) where

import Data.Aeson
import Data.Char (isLower, toLower)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)

data BrockmanConfig = BrockmanConfig
  { configBots :: [BotConfig]
  , configUseTls :: Bool
  , configIrc :: IRCConfig
  , configShortener :: ShortenerConfig
  , configController :: ControllerConfig
  } deriving (Generic)

data ShortenerConfig = ShortenerConfig
  { shortenerUse :: Bool
  , shortenerUrl :: Text
  } deriving (Generic)

data ControllerConfig = ControllerConfig
  { controllerNick :: Text
  , controllerChannels :: [Text]
  } deriving (Generic)

data IRCConfig = IrcConfig
  { ircHost :: Text
  , ircPort :: Int
  } deriving (Generic)

data BotConfig = BotConfig
  { botNick :: Text
  , botFeed :: Text
  , botChannels :: [Text]
  , botDelay :: Int
  } deriving (Generic)

bFeed :: Lens' BotConfig Text
bFeed = lens botFeed $ \bot fs -> bot {botFeed = fs}

bDelay :: Lens' BotConfig Int
bDelay = lens botDelay $ \bot d -> bot {botDelay = d}

cBots :: Lens' BrockmanConfig [BotConfig]
cBots = lens configBots $ \config bs -> config {configBots = bs}

bChannels :: Lens' BotConfig [Text]
bChannels = lens botChannels $ \bot cs -> bot {botChannels = cs}

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

instance FromJSON ShortenerConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON ControllerConfig where
  parseJSON = genericParseJSON myOptions

instance ToJSON BrockmanConfig where
  toJSON = genericToJSON myOptions

instance ToJSON BotConfig where
  toJSON = genericToJSON myOptions

instance ToJSON IRCConfig where
  toJSON = genericToJSON myOptions

instance ToJSON ShortenerConfig where
  toJSON = genericToJSON myOptions

instance ToJSON ControllerConfig where
  toJSON = genericToJSON myOptions
