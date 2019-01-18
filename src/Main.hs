{-# LANGUAGE ApplicativeDo, DeriveGeneric, FlexibleContexts, OverloadedStrings,
  RecordWildCards, ScopedTypeVariables #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson
import qualified Data.BloomFilter as Bloom (fromList)
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kirk.Config
import Network.Socket (HostName, PortNumber)
import Options.Applicative

import Bot
import Util

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

brockmanOptions :: Parser BrockmanOptions
brockmanOptions = do
  ircHost <- strArgument (metavar "IRC-HOST" <> help "IRC server address")
  ircPort <-
    option
      auto
      (long "port" <> short 'p' <> metavar "PORT" <> help "IRC server port" <>
       value 6667 <>
       showDefault)
  configFile <- strArgument (metavar "CONFIG-PATH" <> help "config file path")
  shortener <-
    optional $
    strOption
      (long "shortener" <> short 's' <> metavar "URL" <>
       help "shortener for link URLs")
  pure BrockmanOptions {..}

main :: IO ()
main = do
  BrockmanOptions {..} <-
    execParser $
    info
      (helper <*> brockmanOptions)
      (fullDesc <> progDesc "Broadcast RSS feeds to IRC")
  config <- decode <$> LBS8.readFile configFile
  let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
  bloom <- atomically $ newTVar bloom0
  forConcurrently_ (maybe [] c_bots config) $ \bot ->
    eloop $
    botThread
      bloom
      bot
      shortener
      Config
        { nick = b_nick bot
        , msgtarget = maybe [] c_channels config
        , server_hostname = ircHost
        , server_port = ircPort
        }
  forever $ sleepSeconds 1
