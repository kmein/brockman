{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}
module Brockman.Bot.Reporter where

import Brockman.Bot (handshake, withIrcConnection)
import Brockman.Feed
import Brockman.Types
import Brockman.Util (sleepSeconds, debug, notice)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad (forM_, unless, forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import Data.Conduit
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro
import Network.HTTP.Client (HttpExceptionContent(ConnectionFailure, StatusCodeException), HttpException(HttpExceptionRequest))
import Network.Socket (HostName)
import Network.Wreq (FormParam((:=)), get, post, responseBody, responseStatus, statusCode, statusMessage)
import System.Random (randomRIO)
import Text.Feed.Import (parseFeedSource)
import qualified Control.Exception as E
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Text as T (Text, pack, unpack, words, unwords)
import qualified Network.IRC.Conduit as IRC


data ReporterMessage
  = Pinged (IRC.ServerName BS.ByteString)
  | NewFeedItem FeedItem
  | Exception T.Text


reporterThread :: TVar (Bloom BS.ByteString) -> T.Text -> BotConfig -> BrockmanConfig -> IO ()
reporterThread bloom nick bot@BotConfig {..} config@BrockmanConfig {..} =
  withIrcConnection config listenForPing $ \chan -> do
    handshake nick botChannels
    _ <- liftIO $ forkIO $ feedThread bot True bloom chan
    forever $
      liftIO (readChan chan) >>= \case
        Pinged serverName -> do
          debug botFeed ("pong " <> show serverName)
          yield $ IRC.Pong serverName
        NewFeedItem item -> do
           item' <- liftIO $ maybe (pure item) (\url -> item `shortenWith` T.unpack url) configShortener
           notice botFeed ("sending " <> show (display item'))
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


feedThread :: BotConfig -> Bool -> TVar (Bloom BS.ByteString) -> Chan ReporterMessage -> IO ()
feedThread bot@BotConfig {..} isFirstTime bloom chan = do
    let delaySeconds = fromMaybe 300 botDelay
    liftIO $ when isFirstTime $ do
      randomDelay <- randomRIO (0, delaySeconds)
      debug botFeed ("sleep " <> show randomDelay)
      sleepSeconds randomDelay
    r <- E.try $ get $ T.unpack botFeed
    debug botFeed "fetch"
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
          debug botFeed ("exception" <> T.unpack message)
          writeChan chan (Exception message)
      Right resp -> do
        items <-
          liftIO
          $  atomically
          $  deduplicate bloom
          $  feedToItems
          $  parseFeedSource
          $  resp
          ^. responseBody
        unless isFirstTime $ writeList2Chan chan $ map NewFeedItem items
    liftIO $ sleepSeconds delaySeconds
    debug botFeed "tick"
    feedThread bot False bloom chan

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  debug "" ("Shortening " <> show item <> " with " <> show url)
  r <- post url ["uri" := itemLink item]
  pure item { itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody }
