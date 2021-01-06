{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Brockman.Bot.Reporter where

import Brockman.Bot (handshake, withIrcConnection)
import Brockman.Feed
import Brockman.Types
import Brockman.Util
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forM_, unless, forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import Data.Conduit
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Control.Lens
import Network.HTTP.Client (HttpExceptionContent(ConnectionFailure, StatusCodeException), HttpException(HttpExceptionRequest))
import Network.Socket (HostName)
import Network.Wreq (FormParam((:=)), get, post, responseBody, responseStatus, statusCode, statusMessage)
import System.Random (randomRIO)
import Text.Feed.Import (parseFeedSource)
import qualified Control.Exception as E
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Map as M
import qualified Data.Text as T (Text, pack, unpack, words, unwords)
import qualified Network.IRC.Conduit as IRC


data ReporterMessage
  = Pinged (IRC.ServerName BS.ByteString)
  | NewFeedItem FeedItem
  | Exception T.Text


currentBotConfig :: T.Text -> MVar BrockmanConfig -> IO (Maybe BotConfig)
currentBotConfig nick configMVar = do
  BrockmanConfig{..} <- readMVar configMVar
  return $ M.lookup nick configBots


reporterThread :: MVar (Bloom BS.ByteString) -> MVar BrockmanConfig -> T.Text -> IO ()
reporterThread bloom configMVar nick = do
  config@BrockmanConfig{..} <- readMVar configMVar
  withIrcConnection config listenForPing $ \chan -> do
    liftIO (currentBotConfig nick configMVar) >>= \case
      Nothing -> liftIO suicide
      Just BotConfig {..} -> do
        handshake nick botChannels
        _ <- liftIO $ forkIO $ feedThread nick configMVar True bloom chan
        forever $ do
          liftIO (currentBotConfig nick configMVar) >>= \case
            Nothing -> liftIO suicide
            Just BotConfig{..} ->
              liftIO (readChan chan) >>= \case
                Pinged serverName -> do
                  debug nick ("pong " <> show serverName)
                  yield $ IRC.Pong serverName
                NewFeedItem item -> do
                   item' <- liftIO $ maybe (pure item) (\url -> item `shortenWith` T.unpack url) configShortener
                   notice nick ("sending " <> show (display item'))
                   forM_ botChannels $ \channel ->
                      yield $ IRC.Privmsg (encodeUtf8 channel) $ Right $ encodeUtf8 $ display item'
                Exception message ->
                  forM_ botChannels $ \channel ->
                    yield $ IRC.Notice (encodeUtf8 channel) $ Right $ encodeUtf8 message
  where
    listenForPing chan = forever $ do
      maybeMessage <- await
      case maybeMessage of
        Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> do
          debug "" ("Pinged by " <> show s)
          liftIO $ writeChan chan (Pinged s)
        _ -> pure ()


feedThread :: T.Text -> MVar BrockmanConfig -> Bool -> MVar (Bloom BS.ByteString) -> Chan ReporterMessage -> IO ()
feedThread nick configMVar isFirstTime bloom chan = currentBotConfig nick configMVar >>= \case
  Nothing -> pure ()
  Just BotConfig{..} -> do
    let delaySeconds = fromMaybe 300 botDelay
    liftIO $ when isFirstTime $ do
      randomDelay <- randomRIO (0, delaySeconds)
      debug nick ("sleep " <> show randomDelay)
      sleepSeconds randomDelay
    r <- E.try $ get $ T.unpack botFeed
    debug nick ("fetch " <> T.unpack botFeed)
    case r of
      Left exception ->
        let
          mircRed text = "\ETX4,99" <> text <> "\ETX" -- ref https://www.mirc.com/colors.html
          message = (<> " — " <> botFeed) $ mircRed $ T.unwords $ T.words $ case exception of
            HttpExceptionRequest _ (StatusCodeException response _) ->
              T.unwords [T.pack $ show $ response ^. responseStatus . statusCode, decodeUtf8 $ response ^. responseStatus . statusMessage]
            HttpExceptionRequest _ (ConnectionFailure _) -> "Connection failure"
            _ -> T.pack $ show exception
         in do
          debug nick ("exception" <> T.unpack message)
          writeChan chan (Exception message)
      Right resp -> do
        items <-
          liftIO
          $  deduplicate bloom
          $  feedToItems
          $  parseFeedSource
          $  resp
          ^. responseBody
        unless isFirstTime $ writeList2Chan chan $ map NewFeedItem items
    liftIO $ sleepSeconds delaySeconds
    debug nick "tick"
    feedThread nick configMVar False bloom chan

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  debug "" ("Shortening " <> show item <> " with " <> show url)
  r <- post url ["uri" := itemLink item]
  pure item { itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody }
