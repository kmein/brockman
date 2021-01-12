{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Bot.Controller where

import Brockman.Bot
import Brockman.Bot.Reporter (reporterThread)
import Brockman.Types
import Brockman.Util
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
import qualified Network.IRC.Conduit as IRC
import Safe (readMay)

data ControllerCommand
  = Info (IRC.ChannelName T.Text) (IRC.NickName T.Text)
  | Pinged (IRC.ServerName ByteString)
  | Move (IRC.NickName T.Text) T.Text
  | Add (IRC.NickName T.Text) T.Text (Maybe (IRC.ChannelName ByteString))
  | Remove (IRC.NickName T.Text)
  | Tick (IRC.NickName T.Text) Int
  | Invite (IRC.ChannelName ByteString)
  | Kick (IRC.ChannelName ByteString)
  | Help (IRC.ChannelName T.Text)
  | Subscribe (IRC.NickName T.Text) {- subscriber -} (IRC.NickName T.Text) {- bot -}
  | Unsubscribe (IRC.NickName T.Text) {- subscriber -} (IRC.NickName T.Text) {- bot -}
  | Dump (IRC.ChannelName T.Text)
  deriving (Show)

controllerThread :: MVar (Bloom ByteString) -> MVar BrockmanConfig -> IO ()
controllerThread bloom configMVar = do
  config@BrockmanConfig {configBots, configController, configChannel, configPastebin} <- readMVar configMVar
  forM_ (M.keys configBots) $ \nick ->
    forkIO $ eloop $ reporterThread bloom configMVar nick
  case configController of
    Nothing -> pure ()
    Just ControllerConfig {controllerNick, controllerExtraChannels} ->
      let controllerChannels = configChannel : fromMaybe [] controllerExtraChannels
          listen chan =
            forever $
              await >>= \case
                Just (Right (IRC.Event _ _ (IRC.Invite channel _))) -> liftIO $ writeChan chan (Invite channel)
                Just (Right (IRC.Event _ _ (IRC.Kick channel nick _))) | decodeUtf8 nick == controllerNick -> liftIO $ writeChan chan (Kick channel)
                Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ writeChan chan (Pinged s)
                Just (Right (IRC.Event _ (IRC.User user) (IRC.Privmsg _ (Right message)))) ->
                  liftIO $ case T.words $ decodeUtf8 message of
                    ["subscribe", nick] -> writeChan chan $ Subscribe (decodeUtf8 user) nick
                    ["unsubscribe", nick] -> writeChan chan $ Unsubscribe (decodeUtf8 user) nick
                    ["help"] -> writeChan chan (Help (decodeUtf8 user))
                    ["info", nick] -> writeChan chan (Info (decodeUtf8 user) nick)
                    ["dump"] -> writeChan chan (Dump (decodeUtf8 user))
                    _ -> pure ()
                Just (Right (IRC.Event _ (IRC.Channel channel _) (IRC.Privmsg _ (Right message)))) ->
                  liftIO $ case T.words <$> T.stripPrefix (controllerNick <> ":") (decodeUtf8 message) of
                    Just ["dump"] -> writeChan chan (Dump (decodeUtf8 channel))
                    Just ["help"] -> writeChan chan (Help (decodeUtf8 channel))
                    Just ["info", nick] -> writeChan chan (Info (decodeUtf8 channel) nick)
                    Just ["move", nick, url] -> writeChan chan (Move nick url)
                    Just ["add", nick, url] | "http" `T.isPrefixOf` url && isValidIrcNick nick ->
                      writeChan chan $ Add nick url $
                        if decodeUtf8 channel == configChannel then Nothing else Just channel
                    Just ["remove", nick] -> writeChan chan (Remove nick)
                    Just ["tick", nick, tickString]
                      | Just tick <- readMay (T.unpack tickString) -> writeChan chan (Tick nick tick)
                    Just _ -> writeChan chan (Help (decodeUtf8 channel))
                    _ -> pure ()
                _ -> pure ()
          speak chan = do
            handshake controllerNick controllerChannels
            forever $ do
              config@BrockmanConfig {configBots, configController, configChannel} <- liftIO (readMVar configMVar)
              case configController of
                Nothing -> pure ()
                Just ControllerConfig {controllerNick, controllerExtraChannels} -> do
                  let controllerChannels = configChannel : fromMaybe [] controllerExtraChannels
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
                          "dump — upload the current config/state somewhere you can see it",
                          "/msg " <> controllerNick <> " subscribe NICK — subscribe to private messages from a bot",
                          "/msg " <> controllerNick <> " unsubscribe NICK — unsubscribe to private messages from a bot",
                          "/invite NICK — invite a bot from your channel",
                          "/kick NICK — remove a bot from your channel",
                          "/invite " <> controllerNick <> " — invite the controller to your channel",
                          "/kick " <> controllerNick <> " — kick the controller from your channel"
                        ]
                    Tick nick tick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botDelayL ?~ tick
                      notice nick ("change tick speed to " <> show tick)
                      broadcastNotice controllerChannels $ nick <> " @ " <> T.pack (show tick) <> " seconds"
                    Add nick url extraChannel ->
                      case M.lookup nick configBots of
                        Just BotConfig{botFeed} -> broadcast [maybe configChannel decodeUtf8 extraChannel] [nick <> " is already serving " <> botFeed]
                        Nothing -> do
                          liftIO $ update configMVar $ configBotsL . at nick ?~ BotConfig {botFeed = url, botDelay = Nothing, botExtraChannels = (:[]) . decodeUtf8 <$> extraChannel}
                          _ <- liftIO $ forkIO $ eloop $ reporterThread bloom configMVar nick
                          pure ()
                    Remove nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick .~ Nothing
                      notice nick "remove"
                      broadcastNotice controllerChannels $ nick <> " is expected to commit suicide soon"
                    Move nick url -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botFeedL .~ url
                      notice nick ("move to " <> T.unpack url)
                      broadcastNotice controllerChannels $ nick <> " -> " <> url
                    Pinged serverName -> do
                      debug controllerNick ("pong " <> show serverName)
                      yield $ IRC.Pong serverName
                    Kick channel -> do
                      let channel' = decodeUtf8 channel
                      liftIO $ update configMVar $ configControllerL . mapped . controllerExtraChannelsL %~ delete channel'
                      notice controllerNick $ "kicked from " <> T.unpack channel'
                    Invite channel -> do
                      let channel' = decodeUtf8 channel
                      liftIO $ update configMVar $ configControllerL . mapped . controllerExtraChannelsL %~ insert channel'
                      notice controllerNick $ "invited to " <> T.unpack channel'
                      yield $ IRC.Join channel
                    Subscribe user nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botExtraChannelsL %~ insert user
                      notice nick $ T.unpack user <> " has subscribed"
                      broadcast [user] ["subscribed to " <> nick]
                    Unsubscribe user nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botExtraChannelsL %~ delete user
                      notice nick $ T.unpack user <> " has unsubscribed"
                      broadcast [user] ["unsubscribed from " <> nick]
                    Dump channel ->
                      case configPastebin of
                        Just endpoint -> do
                          url <- liftIO $ pasteJson endpoint config
                          broadcast [channel] [url]
                        Nothing ->
                          broadcast [channel] ["No pastebin set"]
                    Info channel nick -> do
                      broadcast [channel] $
                        pure $
                          case M.lookup nick configBots of
                            Just BotConfig {botFeed, botExtraChannels, botDelay} -> do
                              T.unwords $ [botFeed, T.pack (show (configChannel : fromMaybe [] botExtraChannels))] ++ maybeToList (T.pack . show <$> botDelay)
                            _
                              | nick == controllerNick -> T.pack (show controllerChannels)
                              | otherwise ->
                                case M.keys $ M.filter (\BotConfig{botExtraChannels} -> nick `elem` fromMaybe [] botExtraChannels) configBots of
                                  [] -> nick <> " is neither bot nor subscriber"
                                  subscriptions -> nick <> " has subscribed to " <> T.pack (show subscriptions)
       in withIrcConnection config listen speak
