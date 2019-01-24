{-# LANGUAGE ApplicativeDo, FlexibleContexts, OverloadedStrings,
  RecordWildCards, ScopedTypeVariables #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson
import qualified Data.BloomFilter as Bloom (fromList)
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile)
import Options.Applicative
import System.IO (hPutStrLn, stderr)

import Brockman.Bot
import Brockman.Types
import Brockman.Util (sleepSeconds)

brockmanOptions :: Parser FilePath
brockmanOptions = strArgument $ metavar "CONFIG-PATH" <> help "config file path"

main :: IO ()
main = do
  configFile <-
    execParser $
    info
      (helper <*> brockmanOptions)
      (fullDesc <> progDesc "Broadcast RSS feeds to IRC")
  configJSON <- LBS8.readFile configFile
  case eitherDecode configJSON of
    Right config@BrockmanConfig {..} -> do
      let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
      bloom <- atomically $ newTVar bloom0
      runControllerBot bloom config `race_`
        forConcurrently_
          configBots
          (\bot -> runNewsBot bloom bot config)
      forever $ sleepSeconds 1
    Left err -> hPutStrLn stderr err
