{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Brockman.Bot.Controller (controllerThread)
import Brockman.Types
import Brockman.Util (eloop, sleepSeconds)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr)
import System.Log.Logger
import Text.Read

data BrockmanOptions = BrockmanOptions {configPath :: FilePath, checkOnly :: Bool}

brockmanOptions :: Parser BrockmanOptions
brockmanOptions = do
  configPath <- strArgument $ metavar "CONFIG-PATH" <> help "config file path"
  checkOnly <- switch $ help "stop after validating the config" <> long "check" <> short 'c'
  pure BrockmanOptions {..}

main :: IO ()
main = do
  logLevelString <- lookupEnv "BROCKMAN_LOG_LEVEL"
  let logLevel = fromMaybe NOTICE $ readMaybe =<< logLevelString
  updateGlobalLogger "brockman" (setLevel logLevel)

  hSetBuffering stderr LineBuffering
  options <-
    execParser $
      info
        (helper <*> brockmanOptions)
        (fullDesc <> progDesc "Broadcast RSS feeds to IRC")
  configJSON <- LBS8.readFile $ configPath options
  case eitherDecode configJSON of
    Right config -> do
      if checkOnly options
        then print config
        else do
          debugM "brockman" (show config)
          stateFile <- statePath config
          stateFileExists <- doesFileExist stateFile
          config' <-
            if stateFileExists
              then do
                configJSON' <- LBS8.readFile stateFile
                case eitherDecode configJSON' of
                  Right config' -> config' <$ warningM [] "Parsed state file, resuming"
                  Left _ -> config <$ warningM [] "State file is corrupt, reverting to config"
              else config <$ warningM [] "No state file exists yet, starting with config"
          configMVar <- newMVar config'
          forkIO $ forever $ do
            encodeFile stateFile =<< readMVar configMVar
            sleepSeconds $ fromMaybe 300 $ configStateSaveInterval config'
          eloop $ controllerThread configMVar
          forever $ sleepSeconds 1
    Left err -> errorM "brockman.main" err >> exitFailure
