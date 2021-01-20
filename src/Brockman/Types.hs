{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Brockman.Types where

import Control.Concurrent.MVar
import Control.Lens
import Data.Aeson hiding ((.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isLower, toLower)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

configBotsL :: Lens' BrockmanConfig (Map Text BotConfig)
configBotsL = lens configBots (\config bots -> config {configBots = bots})

configControllerL :: Lens' BrockmanConfig (Maybe ControllerConfig)
configControllerL = lens configController (\config controller -> config {configController = controller})

controllerExtraChannelsL :: Lens' ControllerConfig (Maybe [Text])
controllerExtraChannelsL = lens controllerExtraChannels (\controller channels -> controller {controllerExtraChannels = channels})

botFeedL :: Lens' BotConfig Text
botFeedL = lens botFeed (\bot feed -> bot {botFeed = feed})

botDelayL :: Lens' BotConfig (Maybe Integer)
botDelayL = lens botDelay (\bot delay -> bot {botDelay = delay})

botExtraChannelsL :: Lens' BotConfig (Maybe [Text])
botExtraChannelsL = lens botExtraChannels (\bot channels -> bot {botExtraChannels = channels})

data BrockmanConfig = BrockmanConfig
  { configBots :: Map Text BotConfig,
    configChannel :: Text,
    configUseTls :: Maybe Bool,
    configIrc :: IrcConfig,
    configShortener :: Maybe Text,
    configController :: Maybe ControllerConfig,
    configStatePath :: Maybe FilePath,
    configPastebin :: Maybe Text,
    configDefaultDelay :: Maybe Integer
  }
  deriving (Generic, Show, Typeable)

data ControllerConfig = ControllerConfig
  { controllerNick :: Text,
    controllerExtraChannels :: Maybe [Text]
  }
  deriving (Generic, Show, Typeable)

data IrcConfig = IrcConfig
  { ircHost :: Text,
    ircPort :: Maybe Int
  }
  deriving (Generic, Show, Typeable)

data BotConfig = BotConfig
  { botFeed :: Text,
    botExtraChannels :: Maybe [Text],
    botDelay :: Maybe Integer
  }
  deriving (Generic, Show, Typeable)

statePath :: BrockmanConfig -> IO FilePath
statePath = maybe defaultStatePath pure . configStatePath
  where
    defaultStatePath = (</> "brockman.json") <$> getHomeDirectory

update :: MVar BrockmanConfig -> (BrockmanConfig -> BrockmanConfig) -> IO ()
update stateMVar function = modifyMVar_ stateMVar $ \state ->
  let state' = function state
   in state' <$ dump state'
  where
    dump config = do
      path <- statePath config
      BL.writeFile path $ encodePretty config

myOptions :: Options
myOptions =
  defaultOptions
    { fieldLabelModifier = uncapitalize . dropWhile isLower,
      omitNothingFields = True
    }
  where
    uncapitalize =
      \case
        [] -> []
        (x : xs) -> toLower x : xs

instance FromJSON BrockmanConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON BotConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON IrcConfig where
  parseJSON = genericParseJSON myOptions

instance FromJSON ControllerConfig where
  parseJSON = genericParseJSON myOptions

instance ToJSON BrockmanConfig where
  toJSON = genericToJSON myOptions

instance ToJSON BotConfig where
  toJSON = genericToJSON myOptions

instance ToJSON IrcConfig where
  toJSON = genericToJSON myOptions

instance ToJSON ControllerConfig where
  toJSON = genericToJSON myOptions
