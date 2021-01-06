{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase, TemplateHaskell, TypeFamilies #-}

module Brockman.Types
  ( BrockmanConfig(..)
  , BotConfig(..)
  , ControllerConfig(..)
  , IRCConfig(..)
  , RemoveNick(..)
  , MoveNick(..)
  , AddNick(..)
  , TickNick(..)
  , GetConfig(..)
  ) where

import Data.Acid
import Data.Aeson hiding ((.=))
import Data.Char (isLower, toLower)
import Data.Map (Map)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Control.Lens
import Control.Monad.Reader.Class (MonadReader(ask))
import qualified Network.IRC.Conduit as IRC

configBotsL :: Lens' BrockmanConfig (Map Text BotConfig)
configBotsL = lens configBots (\config bots -> config { configBots = bots })

botFeedL :: Lens' BotConfig Text
botFeedL = lens botFeed (\bot feed -> bot { botFeed = feed })

botDelayL :: Lens' BotConfig (Maybe Int)
botDelayL = lens botDelay (\bot delay -> bot { botDelay = delay })

removeNick :: IRC.NickName Text -> Update BrockmanConfig ()
removeNick nick =
  configBotsL.at nick .= Nothing

moveNick :: IRC.NickName Text -> Text -> Update BrockmanConfig ()
moveNick nick url =
  configBotsL.at nick.mapped.botFeedL .= url

addNick :: IRC.NickName Text -> Text -> [IRC.ChannelName Text] -> Update BrockmanConfig ()
addNick nick url channels =
  configBotsL.at nick ?= BotConfig {botFeed = url, botDelay = Nothing, botChannels = channels}

tickNick :: IRC.NickName Text -> Int -> Update BrockmanConfig ()
tickNick nick tick =
  configBotsL.at nick.mapped.botDelayL ?= tick

getConfig :: Query BrockmanConfig BrockmanConfig
getConfig = ask

data BrockmanConfig = BrockmanConfig
  { configBots :: Map Text BotConfig
  , configUseTls :: Maybe Bool
  , configIrc :: IRCConfig
  , configShortener :: Maybe Text
  , configController :: Maybe ControllerConfig
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


$(deriveSafeCopy 0 'base ''ControllerConfig)
$(deriveSafeCopy 0 'base ''IRCConfig)
$(deriveSafeCopy 0 'base ''BotConfig)
$(deriveSafeCopy 0 'base ''BrockmanConfig)
$(makeAcidic ''BrockmanConfig ['tickNick, 'addNick, 'moveNick, 'removeNick, 'getConfig])


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
