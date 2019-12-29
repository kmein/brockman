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
import           Options.Applicative
import           System.IO                      ( hSetBuffering
                                                , stderr
                                                , BufferMode(LineBuffering)
                                                )
import           System.Log.Formatter
import           System.Log.Handler      hiding ( setLevel )
import           System.Log.Handler.Simple
import           System.Log.Logger

import           Brockman.Bot
import           Brockman.Types
import           Brockman.Util                  ( sleepSeconds )

brockmanOptions :: Parser FilePath
brockmanOptions =
  strArgument $ metavar "CONFIG-PATH" <> help "config file path"

main :: IO ()
main = do
  updateGlobalLogger "brockman" (setLevel DEBUG)
  -- h <-
  --   flip setFormatter (simpleLogFormatter "$time [$prio] $msg")
  --     <$> streamHandler stderr DEBUG
  -- updateGlobalLogger "brockman" (setHandlers [h])

  hSetBuffering stderr LineBuffering
  configFile <- execParser $ info
    (helper <*> brockmanOptions)
    (fullDesc <> progDesc "Broadcast RSS feeds to IRC")
  configJSON <- LBS8.readFile configFile
  debugM "brockman.main"
         ("Read " <> show configJSON <> " from " <> show configFile)
  case eitherDecode configJSON of
    Right config@BrockmanConfig {..} -> do
      infoM "brockman.main" ("Successfully parsed config: " <> show config)
      let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
      bloom <- atomically $ newTVar bloom0
      forConcurrently_ configBots (\bot -> botThread bloom bot config)
      forever $ sleepSeconds 1
    Left err -> errorM "brockman.main" err
