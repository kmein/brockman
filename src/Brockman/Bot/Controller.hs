{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}
module Brockman.Bot.Controller where

import Brockman.Bot (handshake, withIrcConnection)
import Brockman.Bot.Reporter (reporterThread)
import Brockman.Util (debug, eloop)
import Brockman.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.IRC.Conduit as IRC

data ControllerCommand = Info T.Text | Pinged (IRC.ServerName ByteString)

controllerThread :: MVar (Bloom ByteString) -> MVar BrockmanConfig -> IO ()
controllerThread bloom configMVar = do
  config@BrockmanConfig{..} <- readMVar configMVar
  case configController of
    Nothing -> pure ()
    Just controller ->
      let
        listen chan = forever $ await >>= \case
          Just (Right (IRC.Event _ _ (IRC.Privmsg _ (Right message)))) ->
            case T.words $ decodeUtf8 message of
              ["info", nick] -> liftIO $ writeChan chan (Info nick)
              _ -> pure ()
          _ -> pure ()
        speak chan = do
          handshake (controllerNick controller) (controllerChannels controller)
          forever $ liftIO (readChan chan) >>= \case
            Pinged serverName -> do
              debug (controllerNick controller) ("pong " <> show serverName)
              yield $ IRC.Pong serverName
            Info nick ->
              forM_ (controllerChannels controller) $ \channel ->
                yield . IRC.Privmsg (encodeUtf8 channel) . Right . encodeUtf8 $
                  case M.lookup nick configBots of
                    Just BotConfig{..} -> do
                      T.unwords $ [botFeed, T.pack (show botChannels)] ++ maybeToList (T.pack . show <$> botDelay)
                    _ | nick == controllerNick controller -> "https://github.com/kmein/brockman"
                      | otherwise -> nick <> "? Never heard of him."
       in do
         forkIO $ withIrcConnection config listen speak
         forConcurrently_
           (M.toList configBots)
           (\(nick, bot) -> eloop $ reporterThread bloom configMVar nick)
