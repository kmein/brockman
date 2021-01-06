{-# LANGUAGE ApplicativeDo, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Brockman.Bot.Controller (controllerThread)
import Brockman.Types
import Brockman.Util (eloop, sleepSeconds, debug)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Aeson
import Data.BloomFilter.Hash (cheapHashes)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, stderr, BufferMode(LineBuffering))
import System.Log.Logger
import Text.Read
import qualified Data.BloomFilter as Bloom (fromList)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile)

brockmanOptions :: Parser FilePath
brockmanOptions =
  strArgument $ metavar "CONFIG-PATH" <> help "config file path"

main :: IO ()
main = do
  logLevelString <- lookupEnv "BROCKMAN_LOG_LEVEL"
  let logLevel = fromMaybe NOTICE $ readMaybe =<< logLevelString
  updateGlobalLogger "brockman" (setLevel logLevel)

  hSetBuffering stderr LineBuffering
  configFile <- execParser $ info
    (helper <*> brockmanOptions)
    (fullDesc <> progDesc "Broadcast RSS feeds to IRC")
  configJSON <- LBS8.readFile configFile
  case eitherDecode configJSON of
    Right config -> do
      debug "" (show config)
      let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
      stateFile <- statePath config
      stateFileExists <- doesFileExist stateFile
      bloom <- newMVar bloom0
      config' <-
        if stateFileExists
           then do
             configJSON' <- LBS8.readFile stateFile
             case eitherDecode configJSON' of
               Right config' -> config' <$ warningM [] "Parsed state file, resuming"
               Left _ -> config <$ warningM [] "State file is corrupt, reverting to config"
           else config <$ warningM [] "No state file exists yet, starting with config"
      eloop $ controllerThread bloom =<< newMVar config'
      forever $ sleepSeconds 1
    Left err -> errorM "brockman.main" err
