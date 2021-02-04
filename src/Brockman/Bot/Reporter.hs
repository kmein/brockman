{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Bot.Reporter where

import Brockman.Bot
import Brockman.Feed
import Brockman.Types
import Brockman.Util
import Control.Applicative (Alternative (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Cache.LRU as LRU
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Conduit
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text, pack, unpack, unwords, words)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (ConnectionFailure, StatusCodeException))
import qualified Network.IRC.Conduit as IRC
import Network.Socket (HostName)
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody, responseStatus, statusCode, statusMessage)
import System.Log.Logger
import System.Random (randomRIO)
import Text.Feed.Import (parseFeedSource)

data ReporterMessage
  = Pinged (IRC.ServerName BS.ByteString)
  | Invited Channel
  | Kicked Channel
  | NewFeedItem FeedItem
  | Exception T.Text
  deriving (Show)

-- return the current config or kill thread if the key is not present
withCurrentBotConfig :: MonadIO m => Nick -> MVar BrockmanConfig -> (BotConfig -> m ()) -> m ()
withCurrentBotConfig nick configMVar handler = do
  BrockmanConfig {configBots} <- liftIO $ readMVar configMVar
  maybe (liftIO suicide) handler $ M.lookup nick configBots

reporterThread :: MVar BrockmanConfig -> Nick -> IO ()
reporterThread configMVar nick = do
  config@BrockmanConfig {configChannel, configShortener} <- readMVar configMVar
  withIrcConnection config listen $ \chan -> do
    withCurrentBotConfig nick configMVar $ \initialBotConfig -> do
      handshake nick $ configChannel : fromMaybe [] (botExtraChannels initialBotConfig)
      deafen nick
      _ <- liftIO $ forkIO $ feedThread nick configMVar True Nothing chan
      forever $
        withCurrentBotConfig nick configMVar $ \_ -> do
          channels <- botChannels nick <$> liftIO (readMVar configMVar)
          command <- liftIO (readChan chan)
          debug nick $ show command
          case command of
            Pinged serverName -> do
              debug nick ("pong " <> show serverName)
              yield $ IRC.Pong serverName
            NewFeedItem item -> do
              item' <- liftIO $ maybe (pure item) (\url -> item `shortenWith` T.unpack url) configShortener
              debug nick ("sending " <> show (display item'))
              broadcast channels [display item']
            Exception message ->
              broadcastNotice channels message
            Kicked channel -> do
              liftIO $ update configMVar $ configBotsL . at nick . mapped . botExtraChannelsL %~ delete channel
              notice nick $ "kicked from " <> show channel
            Invited channel -> do
              liftIO $ update configMVar $ configBotsL . at nick . mapped . botExtraChannelsL %~ insert channel
              notice nick $ "invited to " <> show channel
              yield $ IRC.Join $ encode channel
  where
    listen chan =
      forever $
        await >>= \case
          Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ writeChan chan (Pinged s)
          Just (Right (IRC.Event _ _ (IRC.Invite channel _))) ->
            liftIO $ writeChan chan $ Invited $ decode channel
          Just (Right (IRC.Event _ _ (IRC.Kick channel nick' _)))
            | nick == decode nick' ->
              liftIO $ writeChan chan $ Kicked $ decode channel
          _ -> pure ()

getFeed :: URL -> IO (Maybe Integer, Either T.Text [FeedItem])
getFeed url =
  E.try (getWith options (T.unpack url)) >>= \case
    Left exception ->
      let mircRed text = "\ETX4,99" <> text <> "\ETX" -- ref https://www.mirc.com/colors.html
          message = mircRed $
            T.unwords $
              T.words $ case exception of
                HttpExceptionRequest _ (StatusCodeException response _) ->
                  T.unwords [T.pack $ show $ response ^. responseStatus . statusCode, decodeUtf8 $ response ^. responseStatus . statusMessage]
                HttpExceptionRequest _ (ConnectionFailure _) -> "Connection failure"
                _ -> T.pack $ show exception
       in return (Nothing, Left message)
    Right response -> do
      now <- liftIO getCurrentTime
      let feed = parseFeedSource $ response ^. responseBody
          delta = feedEntryDelta now =<< feed
          feedItems = feedToItems feed
      return (delta, Right feedItems)
  where
    options = defaults & header "Accept" .~ ["application/atom+xml", "application/rss+xml", "*/*"]

feedThread :: Nick -> MVar BrockmanConfig -> Bool -> Maybe (LRU.LRU FeedItem val) -> Chan ReporterMessage -> IO ()
feedThread nick configMVar isFirstTime lru chan =
  withCurrentBotConfig nick configMVar $ \BotConfig {botDelay, botFeed} -> do
    defaultDelay <- configDefaultDelay <$> readMVar configMVar
    let delaySeconds = fromMaybe fallbackDelay $ botDelay <|> defaultDelay
    liftIO $
      when isFirstTime $ do
        randomDelay <- randomRIO (0, delaySeconds)
        debug nick $ "sleep " <> show randomDelay
        sleepSeconds randomDelay
    debug nick ("fetch " <> T.unpack botFeed)
    (newTick, exceptionOrFeed) <- liftIO $ getFeed botFeed
    newLRU <- case exceptionOrFeed of
      Left message -> do
        error' nick $ "exception" <> T.unpack message
        writeChan chan $ Exception $ message <> " — " <> botFeed
        return lru
      Right feedItems -> do
        let (lru', items) = deduplicate lru feedItems
        when (null feedItems) $ do
          warning nick $ "Feed is empty: " <> T.unpack botFeed
          writeChan chan $ Exception $ "feed is empty: " <> botFeed
        unless isFirstTime $ writeList2Chan chan $ map NewFeedItem items
        return $ Just lru'
    let tick = max 1 $ min 86400 $ fromMaybe fallbackDelay $ botDelay <|> newTick <|> defaultDelay
    notice nick $ "tick " <> show tick
    liftIO $ sleepSeconds tick
    feedThread nick configMVar False newLRU chan
  where
    fallbackDelay = 300

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  debugM "brockman" ("Shortening " <> show item <> " with " <> show url)
  r <- post url ["uri" := itemLink item]
  pure item {itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody}
