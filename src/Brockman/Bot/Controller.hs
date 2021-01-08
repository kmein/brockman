{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Bot.Controller where

import Brockman.Bot
import Brockman.Bot.Reporter (reporterThread)
import Brockman.Types
import Brockman.Util (debug, eloop, notice, decodeUtf8)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.List (delete, insert)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Network.IRC.Conduit as IRC
import Safe (readMay)

data ControllerCommand
  = Info (IRC.ChannelName T.Text) (IRC.NickName T.Text)
  | Pinged (IRC.ServerName ByteString)
  | Move (IRC.NickName T.Text) T.Text
  | Add (IRC.NickName T.Text) T.Text
  | Remove (IRC.NickName T.Text)
  | Tick (IRC.NickName T.Text) Int
  | Invite (IRC.ChannelName ByteString)
  | Kick (IRC.ChannelName ByteString)
  | Help (IRC.ChannelName T.Text)
  deriving (Show)

controllerThread :: MVar (Bloom ByteString) -> MVar BrockmanConfig -> IO ()
controllerThread bloom configMVar = do
  config@BrockmanConfig {configBots, configController} <- readMVar configMVar
  forM_ (M.keys configBots) $ \nick ->
    forkIO $ eloop $ reporterThread bloom configMVar nick
  case configController of
    Nothing -> pure ()
    Just ControllerConfig {controllerNick, controllerChannels} ->
      let listen chan =
            forever $
              await >>= \case
                Just (Right (IRC.Event _ _ (IRC.Invite channel _))) -> liftIO $ writeChan chan (Invite channel)
                Just (Right (IRC.Event _ _ (IRC.Kick channel _ _))) -> liftIO $ writeChan chan (Kick channel)
                Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ writeChan chan (Pinged s)
                Just (Right (IRC.Event _ _ (IRC.Privmsg channel (Right message)))) ->
                  case T.words <$> T.stripPrefix (controllerNick <> ":") (decodeUtf8 message) of
                    Just ["help"] -> liftIO $ writeChan chan (Help (decodeUtf8 channel))
                    Just ["info", nick] -> liftIO $ writeChan chan (Info (decodeUtf8 channel) nick)
                    Just ["move", nick, url] -> liftIO $ writeChan chan (Move nick url)
                    Just ["add", nick, url] -> liftIO $ writeChan chan (Add nick url)
                    Just ["remove", nick] -> liftIO $ writeChan chan (Remove nick)
                    Just ["tick", nick, tickString]
                      | Just tick <- readMay (T.unpack tickString) -> liftIO $ writeChan chan (Tick nick tick)
                    Just _ -> liftIO $ writeChan chan (Help (decodeUtf8 channel))
                    _ -> pure ()
                _ -> pure ()
          speak chan = do
            handshake controllerNick controllerChannels
            forever $ do
              config@BrockmanConfig {configBots, configController} <- liftIO (readMVar configMVar)
              case configController of
                Nothing -> pure ()
                Just ControllerConfig {controllerNick, controllerChannels} -> do
                  command <- liftIO (readChan chan)
                  notice controllerNick (show command)
                  case command of
                    Help channel -> do
                      broadcast
                        [channel]
                        [ "help — send this helpful message",
                          "info NICK — display a bot's settings",
                          "move NICK FEED_URL — change a bot's feed url",
                          "tick NICK SECONDS — change a bot's tick speed",
                          "add NICK FEED_URL — add a new bot to all channels I am in",
                          "remove NICK — tell a bot to commit suicice",
                          "/invite NICK — invite a bot from your channel",
                          "/kick NICK — remove a bot from your channel",
                          "/invite " <> controllerNick <> " — invite the controller to your channel",
                          "/kick " <> controllerNick <> " — kick the controller from your channel"
                        ]
                    Tick nick tick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botDelayL ?~ tick
                      notice nick ("change tick speed to " <> show tick)
                      broadcast controllerChannels [nick <> " @ " <> T.pack (show tick) <> " seconds"]
                    Add nick url -> do
                      liftIO $ update configMVar $ configBotsL . at nick ?~ BotConfig {botFeed = url, botDelay = Nothing, botChannels = controllerChannels}
                      _ <- liftIO $ forkIO $ eloop $ reporterThread bloom configMVar nick
                      pure ()
                    Remove nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick .~ Nothing
                      notice nick "remove"
                      broadcast controllerChannels [nick <> " is expected to commit suicide soon"]
                    Move nick url -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botFeedL .~ url
                      notice nick ("move to " <> T.unpack url)
                      broadcast controllerChannels [nick <> " -> " <> url]
                    Pinged serverName -> do
                      debug controllerNick ("pong " <> show serverName)
                      yield $ IRC.Pong serverName
                    Kick channel -> do
                      let channel' = decodeUtf8 channel
                      liftIO $ update configMVar $ configControllerL . mapped . controllerChannelsL %~ delete channel'
                      notice controllerNick $ "kicked from " <> T.unpack channel'
                    Invite channel -> do
                      let channel' = decodeUtf8 channel
                      liftIO $ update configMVar $ configControllerL . mapped . controllerChannelsL %~ insert channel'
                      notice controllerNick $ "invited to " <> T.unpack channel'
                      yield $ IRC.Join channel
                    Info channel nick -> do
                      broadcast [channel] $
                        pure $
                          case M.lookup nick configBots of
                            Just BotConfig {botFeed, botChannels, botDelay} -> do
                              T.unwords $ [botFeed, T.pack (show botChannels)] ++ maybeToList (T.pack . show <$> botDelay)
                            _
                              | nick == controllerNick -> T.pack (show controllerChannels)
                              | otherwise -> "I don't manage " <> nick
       in withIrcConnection config listen speak
