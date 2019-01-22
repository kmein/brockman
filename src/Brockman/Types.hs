{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase #-}

module Brockman.Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isLower, toLower)
import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Lens.Micro (Lens', lens)

data BrockmanConfig = BrockmanConfig
  { configBots :: [BotConfig]
  , configChannels :: [Text]
  , configUseTls :: Bool
  , configIrc :: IRCConfig
  , configShortener :: ShortenerConfig
  , configController :: Text
  } deriving (Generic)

data ShortenerConfig = ShortenerConfig
  { shortenerUse :: Bool
  , shortenerUrl :: Text
  } deriving (Generic)

data IRCConfig = IrcConfig
  { ircHost :: Text
  , ircPort :: Int
  } deriving (Generic)

data BotConfig = BotConfig
  { botNick :: Text
  , botFeeds :: [Text]
  , botDelay :: Int
  } deriving (Generic)

bNick :: Lens' BotConfig Text
bNick = lens botNick $ \bot n -> bot {botNick = n}

bFeeds :: Lens' BotConfig [Text]
bFeeds = lens botFeeds $ \bot fs -> bot {botFeeds = fs}

bDelay :: Lens' BotConfig Int
bDelay = lens botDelay $ \bot d -> bot {botDelay = d}

cBots :: Lens' BrockmanConfig [BotConfig]
cBots = lens configBots $ \config bs -> config {configBots = bs}

cChannels :: Lens' BrockmanConfig [Text]
cChannels = lens configChannels $ \config cs -> config {configChannels = cs}

myParseJson :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
myParseJson =
  genericParseJSON
    defaultOptions {fieldLabelModifier = uncapitalize . dropWhile isLower}
  where
    uncapitalize =
      \case
        [] -> []
        (x:xs) -> toLower x : xs

instance FromJSON BrockmanConfig where
  parseJSON = myParseJson

instance FromJSON BotConfig where
  parseJSON = myParseJson

instance FromJSON IRCConfig where
  parseJSON = myParseJson

instance FromJSON ShortenerConfig where
  parseJSON = myParseJson
