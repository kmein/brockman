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
import Data.CaseInsensitive (foldedCase, mk)
import Data.Conduit
import Data.Foldable (find)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Network.IRC.Conduit as IRC
import Safe (readMay)

data ControllerCommand
  = Info Channel Nick
  | Pinged (IRC.ServerName ByteString)
  | SetUrl Nick URL
  | Add Nick URL (Maybe Channel)
  | Remove Nick
  | Tick Nick (Maybe Integer)
  | Invite Channel
  | Kick Channel
  | Help Channel
  | Subscribe Nick {- subscriber -} Nick {- bot -}
  | Unsubscribe Nick {- subscriber -} Nick {- bot -}
  | Dump Channel
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
                Just (Right (IRC.Event _ _ (IRC.Invite channel _))) -> liftIO $ writeChan chan $ Invite $ mk $ decodeUtf8 channel
                Just (Right (IRC.Event _ _ (IRC.Kick channel nick _))) | mk (decodeUtf8 nick) == controllerNick -> liftIO $ writeChan chan $ Kick $ mk $ decodeUtf8 channel
                Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ writeChan chan (Pinged s)
                Just (Right (IRC.Event _ (IRC.User user) (IRC.Privmsg _ (Right message)))) ->
                  liftIO $ case T.words $ decodeUtf8 message of
                    ["subscribe", nick] -> writeChan chan $ Subscribe (mk $ decodeUtf8 user) (mk nick)
                    ["unsubscribe", nick] -> writeChan chan $ Unsubscribe (mk $ decodeUtf8 user) (mk nick)
                    ["help"] -> writeChan chan $ Help $ mk $ decodeUtf8 user
                    ["info", nick] -> writeChan chan $ Info (mk $ decodeUtf8 user) $ mk nick
                    ["dump"] -> writeChan chan $ Dump $ mk $ decodeUtf8 user
                    _ -> pure ()
                Just (Right (IRC.Event _ (IRC.Channel channel _) (IRC.Privmsg _ (Right message)))) ->
                  liftIO $ case T.words <$> T.stripPrefix (foldedCase controllerNick <> ":") (decodeUtf8 message) of
                    Just ["dump"] -> writeChan chan (Dump $ mk $ decodeUtf8 channel)
                    Just ["help"] -> writeChan chan (Help $ mk $ decodeUtf8 channel)
                    Just ["info", nick] -> writeChan chan (Info (mk $ decodeUtf8 channel) $ mk nick)
                    Just ["set-url", nick, url] -> writeChan chan (SetUrl (mk nick) url)
                    Just ["add", nick, url]
                      | "http" `T.isPrefixOf` url && isValidIrcNick nick ->
                        writeChan chan $
                          Add (mk nick) url $
                            if mk (decodeUtf8 channel) == configChannel then Nothing else Just $ mk $ decodeUtf8 channel
                    Just ["remove", nick] -> writeChan chan $ Remove $ mk nick
                    Just ["tick", nick, tickString] ->
                      writeChan chan $ Tick (mk nick) $ readMay $ T.unpack tickString
                    Just _ -> writeChan chan $ Help $ mk $ decodeUtf8 channel
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
                          "set-url NICK FEED_URL — change a bot's feed url",
                          "tick NICK SECONDS — change a bot's tick speed",
                          "add NICK FEED_URL — add a new bot to all channels I am in",
                          "remove NICK — tell a bot to commit suicice",
                          "dump — upload the current config/state somewhere you can see it",
                          "/msg " <> foldedCase controllerNick <> " subscribe NICK — subscribe to private messages from a bot",
                          "/msg " <> foldedCase controllerNick <> " unsubscribe NICK — unsubscribe to private messages from a bot",
                          "/invite NICK — invite a bot from your channel",
                          "/kick NICK — remove a bot from your channel",
                          "/invite " <> foldedCase controllerNick <> " — invite the controller to your channel",
                          "/kick " <> foldedCase controllerNick <> " — kick the controller from your channel"
                        ]
                    Tick nick tick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botDelayL .~ tick
                      notice nick ("change tick speed to " <> show tick)
                      channelsForNick <- botChannels nick <$> liftIO (readMVar configMVar)
                      broadcastNotice channelsForNick $ foldedCase nick <> " @ " <> T.pack (maybe "auto" ((<> " seconds") . show) tick)
                    Add nick url extraChannel ->
                      case M.lookup nick configBots of
                        Just BotConfig {botFeed} -> broadcast [fromMaybe configChannel extraChannel] [foldedCase nick <> " is already serving " <> botFeed]
                        Nothing -> do
                          liftIO $ update configMVar $ configBotsL . at nick ?~ BotConfig {botFeed = url, botDelay = Nothing, botExtraChannels = (: []) <$> extraChannel}
                          _ <- liftIO $ forkIO $ eloop $ reporterThread bloom configMVar nick
                          pure ()
                    Remove nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick .~ Nothing
                      notice nick "remove"
                      channelsForNick <- botChannels nick <$> liftIO (readMVar configMVar)
                      broadcastNotice channelsForNick $ foldedCase nick <> " is expected to commit suicide soon"
                    SetUrl nick url -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botFeedL .~ url
                      notice nick ("set url to " <> T.unpack url)
                      channelsForNick <- botChannels nick <$> liftIO (readMVar configMVar)
                      broadcastNotice channelsForNick $ foldedCase nick <> " -> " <> url
                    Pinged serverName -> do
                      debug controllerNick ("pong " <> show serverName)
                      yield $ IRC.Pong serverName
                    Kick channel -> do
                      liftIO $ update configMVar $ configControllerL . mapped . controllerExtraChannelsL %~ delete channel
                      notice controllerNick $ "kicked from " <> show channel
                    Invite channel -> do
                      liftIO $ update configMVar $ configControllerL . mapped . controllerExtraChannelsL %~ insert channel
                      notice controllerNick $ "invited to " <> show channel
                      yield $ IRC.Join $ encodeUtf8 $ foldedCase channel
                    Subscribe user nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botExtraChannelsL %~ insert user
                      notice nick $ T.unpack (foldedCase user) <> " has subscribed"
                      broadcast [user] ["subscribed to " <> foldedCase nick]
                    Unsubscribe user nick -> do
                      liftIO $ update configMVar $ configBotsL . at nick . mapped . botExtraChannelsL %~ delete user
                      notice nick $ T.unpack (foldedCase user) <> " has unsubscribed"
                      broadcast [user] ["unsubscribed from " <> foldedCase nick]
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
                                case M.keys $ M.filter (\BotConfig {botExtraChannels} -> nick `elem` fromMaybe [] botExtraChannels) configBots of
                                  [] -> foldedCase nick <> " is neither bot nor subscriber"
                                  subscriptions -> foldedCase nick <> " has subscribed to " <> T.pack (show subscriptions)
       in withIrcConnection config listen speak
