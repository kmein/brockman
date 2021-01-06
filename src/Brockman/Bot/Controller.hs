{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Bot.Controller where

import Brockman.Bot
import Brockman.Bot.Reporter (reporterThread)
import Brockman.Types
import Brockman.Util (debug, eloop, notice)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Network.IRC.Conduit as IRC
import Safe (readMay)

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
  config@BrockmanConfig {configBots, configController} <- readMVar configMVar
  forM_ (M.keys configBots) $ \nick ->
    forkIO $ eloop $ reporterThread bloom configMVar nick
  case configController of
    Nothing -> pure ()
    Just ControllerConfig {controllerNick, controllerChannels} ->
      let listen chan =
            forever $
              await >>= \case
                Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ writeChan chan (Pinged s)
                Just (Right (IRC.Event _ _ (IRC.Privmsg _ (Right message)))) ->
                  case T.words <$> T.stripPrefix (controllerNick <> ":") (decodeUtf8 message) of
                    Just ["help"] -> liftIO $ writeChan chan Help
                    Just ["info", nick] -> liftIO $ writeChan chan (Info nick)
                    Just ["move", nick, url] -> liftIO $ writeChan chan (Move nick url)
                    Just ["add", nick, url] -> liftIO $ writeChan chan (Add nick url)
                    Just ["remove", nick] -> liftIO $ writeChan chan (Remove nick)
                    Just ["tick", nick, tickString]
                      | Just tick <- readMay (T.unpack tickString) -> liftIO $ writeChan chan (Tick nick tick)
                    Just _ -> liftIO $ writeChan chan Help
                    _ -> pure ()
                _ -> pure ()
          speak chan = do
            handshake controllerNick controllerChannels
            forever $ do
              config@BrockmanConfig {configBots} <- liftIO (readMVar configMVar)
              command <- liftIO (readChan chan)
              notice controllerNick (show command)
              case command of
                Help -> do
                  broadcast
                    controllerChannels
                    [ "help — send this helpful message",
                      "info NICK — display a bot's settings",
                      "move NICK FEED_URL — change a bot's feed url",
                      "tick NICK SECONDS — change a bot's tick speed",
                      "add NICK FEED_URL — add a new bot to all channels I am in",
                      "remove NICK — tell a bot to commit suicice",
                      "/invite NICK — invite a bot from your channel",
                      "/kick NICK — remove a bot from your channel"
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
                Info nick -> do
                  broadcast controllerChannels $
                    pure $
                      case M.lookup nick configBots of
                        Just BotConfig {botFeed, botChannels, botDelay} -> do
                          T.unwords $ [botFeed, T.pack (show botChannels)] ++ maybeToList (T.pack . show <$> botDelay)
                        _
                          | nick == controllerNick -> "https://github.com/kmein/brockman"
                          | otherwise -> nick <> "? Never heard of him."
       in withIrcConnection config listen speak
