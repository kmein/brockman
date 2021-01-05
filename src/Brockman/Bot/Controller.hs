{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}
module Brockman.Bot.Controller where

import Brockman.Bot (handshake, withIrcConnection)
import Brockman.Util (debug)
import Brockman.Types
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.IRC.Conduit as IRC

data ControllerCommand = Info T.Text | Pinged (IRC.ServerName ByteString)

controllerThread :: BrockmanConfig -> IO ()
controllerThread config@BrockmanConfig{..}
  | Just controller <- configController =
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
                  _ | nick == controllerNick controller -> "I drive the slaves around here."
                    | otherwise -> nick <> "? Never heard of him."
     in withIrcConnection config listen speak
  | otherwise = pure ()
