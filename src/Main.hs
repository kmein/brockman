{-# LANGUAGE DeriveGeneric, FlexibleContexts, OverloadedStrings,
  ScopedTypeVariables, ViewPatterns #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson
import qualified Data.BloomFilter as Bloom (fromList)
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile)
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import GHC.Generics (Generic)
import Kirk.Config
import Safe (readMay)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Bot
import Util

data BrockmanConfig = BrockmanConfig
  { c_bots :: [NewsBot]
  , c_channels :: [Text]
  } deriving (Generic)

instance FromJSON BrockmanConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [ircHost, readMay -> Just ircPort, configFile] -> do
      config <- decode <$> LBS8.readFile configFile
      let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
      bloom <- atomically $ newTVar bloom0
      forConcurrently_ (maybe [] c_bots config) $ \bot ->
        eloop $
        botThread
          bloom
          bot
          Config
            { nick = b_nick bot
            , msgtarget = maybe [] c_channels config
            , server_hostname = ircHost
            , server_port = ircPort
            }
      forever $ sleepSeconds 1
    _ -> do
      programName <- pack <$> getProgName
      hPutStrLn stderr $
        "Usage: " <> programName <> " IRC-SERVER IRC-PORT CONFIG"
      exitFailure
