{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}
module Brockman.Bot.Controller where

import Brockman.Bot (handshake, withIrcConnection)
import Brockman.Bot.Reporter (reporterThread)
import Brockman.Types
import Brockman.Util (notice, debug, eloop)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Maybe
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Safe (readMay)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.IRC.Conduit as IRC

data ControllerCommand
  = Info (IRC.NickName T.Text)
  | Pinged (IRC.ServerName ByteString)
  | Move (IRC.NickName T.Text) T.Text
  | Add (IRC.NickName T.Text) T.Text
  | Remove (IRC.NickName T.Text)
  | Tick (IRC.NickName T.Text) Int
  | Help
  deriving (Show)

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
              ["help"] -> liftIO $ writeChan chan Help
              ["info", nick] -> liftIO $ writeChan chan (Info nick)
              ["move", nick, url] -> liftIO $ writeChan chan (Move nick url)
              ["add", nick, url] -> liftIO $ writeChan chan (Add nick url)
              ["remove", nick] -> liftIO $ writeChan chan (Remove nick)
              ["tick", nick, tickString]
                | Just tick <- readMay (T.unpack tickString) -> liftIO $ writeChan chan (Tick nick tick)
              _ -> pure ()
          _ -> pure ()
        speak chan = do
          handshake (controllerNick controller) (controllerChannels controller)
          forever $ do
            config@BrockmanConfig{..} <- liftIO (readMVar configMVar)
            command <- liftIO (readChan chan)
            notice (controllerNick controller) (show command)
            case command of
              Help -> do
                forM_ (controllerChannels controller) $ \channel ->
                  mapM (yield . IRC.Privmsg (encodeUtf8 channel) . Right . encodeUtf8)
                    [ "help — send this helpful message"
                    , "info NICK — display a bot's settings"
                    , "move NICK FEED_URL — change a bot's feed url"
                    , "tick NICK SECONDS — change a bot's tick speed"
                    , "add NICK FEED_URL — add a new bot to all channels I am in"
                    , "remove NICK — tell a bot to commit suicice"
                    ]
              Tick nick tick -> do
                liftIO $ modifyMVar_ configMVar $ pure . (configBotsL.at nick.mapped.botDelayL ?~ tick)
                notice nick ("change tick speed to " <> show tick)
                forM_ (controllerChannels controller) $ \channel ->
                  yield . IRC.Privmsg (encodeUtf8 channel) . Right . encodeUtf8 $ nick <> " @ " <> T.pack (show tick) <> " seconds"
              Add nick url -> do
                liftIO $ modifyMVar_ configMVar $ pure . (configBotsL.at nick ?~ BotConfig {botFeed = url, botDelay = Nothing, botChannels = controllerChannels controller})
                liftIO $ forkIO $ eloop $ reporterThread bloom configMVar nick
                pure ()
              Remove nick -> do
                liftIO $ modifyMVar_ configMVar $ pure . (configBotsL.at nick .~ Nothing)
                notice nick "remove"
                forM_ (controllerChannels controller) $ \channel ->
                  yield . IRC.Privmsg (encodeUtf8 channel) . Right . encodeUtf8 $ nick <> " is expected to commit suicide soon"
              Move nick url -> do
                liftIO $ modifyMVar_ configMVar $ pure . (configBotsL.at nick.mapped.botFeedL .~ url)
                notice nick ("move to " <> T.unpack url)
                forM_ (controllerChannels controller) $ \channel ->
                  yield . IRC.Privmsg (encodeUtf8 channel) . Right . encodeUtf8 $ nick <> " -> " <> url
              Pinged serverName -> do
                debug (controllerNick controller) ("pong " <> show serverName)
                yield $ IRC.Pong serverName
              Info nick -> do
                forM_ (controllerChannels controller) $ \channel ->
                  yield . IRC.Privmsg (encodeUtf8 channel) . Right . encodeUtf8 $
                    case M.lookup nick configBots of
                      Just BotConfig{..} -> do
                        T.unwords $ [botFeed, T.pack (show botChannels)] ++ maybeToList (T.pack . show <$> botDelay)
                      _ | nick == controllerNick controller -> "https://github.com/kmein/brockman"
                        | otherwise -> nick <> "? Never heard of him."
       in do
         _ <- forkIO $ withIrcConnection config listen speak
         forConcurrently_ (M.keys configBots) $ \nick ->
           eloop $ reporterThread bloom configMVar nick
