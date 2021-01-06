{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase #-}

module Brockman.Types where

import Data.Aeson hiding ((.=))
import Data.Char (isLower, toLower)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.Lens
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)

configBotsL :: Lens' BrockmanConfig (Map Text BotConfig)
configBotsL = lens configBots (\config bots -> config { configBots = bots })

botFeedL :: Lens' BotConfig Text
botFeedL = lens botFeed (\bot feed -> bot { botFeed = feed })

botDelayL :: Lens' BotConfig (Maybe Int)
botDelayL = lens botDelay (\bot delay -> bot { botDelay = delay })

data BrockmanConfig = BrockmanConfig
  { configBots :: Map Text BotConfig
  , configUseTls :: Maybe Bool
  , configIrc :: IRCConfig
  , configShortener :: Maybe Text
  , configController :: Maybe ControllerConfig
  , configStatePath :: Maybe FilePath
  } deriving (Generic, Show, Typeable)

data ControllerConfig = ControllerConfig
  { controllerNick :: Text
  , controllerChannels :: [Text]
  } deriving (Generic, Show, Typeable)

data IRCConfig = IrcConfig
  { ircHost :: Text
  , ircPort :: Maybe Int
  } deriving (Generic, Show, Typeable)

data BotConfig = BotConfig
  { botFeed :: Text
  , botChannels :: [Text]
  , botDelay :: Maybe Int
  } deriving (Generic, Show, Typeable)

statePath :: BrockmanConfig -> IO FilePath
statePath = maybe defaultStatePath pure . configStatePath
  where defaultStatePath = (</> "brockman.json") <$> getHomeDirectory

myOptions :: Options
myOptions = defaultOptions
  { fieldLabelModifier = uncapitalize . dropWhile isLower
  , omitNothingFields = True
  }
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
