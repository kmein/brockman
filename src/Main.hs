{-# LANGUAGE ApplicativeDo, FlexibleContexts, OverloadedStrings,
  RecordWildCards, ScopedTypeVariables #-}

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad                  ( forever )
import           Data.Aeson
import qualified Data.BloomFilter              as Bloom
                                                ( fromList )
import           Data.BloomFilter.Hash          ( cheapHashes )
import qualified Data.ByteString.Lazy.Char8    as LBS8
                                                ( readFile )
import           Data.Map                       ( toList )
import           Data.Maybe                     ( fromMaybe )
import           Options.Applicative
import           System.Environment             ( lookupEnv )
import           System.IO                      ( hSetBuffering
                                                , stderr
                                                , BufferMode(LineBuffering)
                                                )
import           System.Log.Logger
import           Text.Read

import           Brockman.Bot
import           Brockman.Types
import           Brockman.Util                  ( sleepSeconds )

brockmanOptions :: Parser FilePath
brockmanOptions = strArgument $ metavar "CONFIG-PATH" <> help "config file path"

main :: IO ()
main = do
  logLevelString <- lookupEnv "BROCKMAN_LOG_LEVEL"
  let logLevel = fromMaybe NOTICE $ readMaybe =<< logLevelString
  updateGlobalLogger "brockman" (setLevel logLevel)

  hSetBuffering stderr LineBuffering
  configFile <- execParser
    $ info (helper <*> brockmanOptions) (fullDesc <> progDesc "Broadcast RSS feeds to IRC")
  configJSON <- LBS8.readFile configFile
  case eitherDecode configJSON of
    Right config@BrockmanConfig {..} -> do
      debugM "brockman.main" (show config)
      let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
      bloom <- atomically $ newTVar bloom0
      forConcurrently_ (toList configBots) (\(nick, bot) -> botThread bloom nick bot config)
      forever $ sleepSeconds 1
    Left err -> errorM "brockman.main" err
