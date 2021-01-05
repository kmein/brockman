{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, OverloadedStrings #-}

module Brockman.Bot
  ( botThread
  )
where

import           Data.Conduit
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import qualified Control.Exception             as E
import           Control.Monad                  ( forM_
                                                , unless
                                                , forever
                                                , when
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.BloomFilter               ( Bloom )
import qualified Data.ByteString               as BS
                                                ( ByteString )
import qualified Data.ByteString.Lazy          as BL
                                                ( toStrict )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text as T                 ( Text
                                                , pack
                                                , unpack
                                                , words
                                                , unwords
                                                )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Lens.Micro
import qualified Network.IRC.Conduit           as IRC
import           Network.HTTP.Client            ( HttpExceptionContent(ConnectionFailure, StatusCodeException)
                                                , HttpException(HttpExceptionRequest)
                                                )
import           Network.Socket                 ( HostName )
import           Network.Wreq                   ( FormParam((:=))
                                                , get
                                                , post
                                                , responseBody
                                                , responseStatus
                                                , statusCode
                                                , statusMessage
                                                )
import           System.Random                  ( randomRIO )
import           Text.Feed.Import               ( parseFeedSource )

import           Brockman.Feed
import           Brockman.Types
import           Brockman.Util                  ( sleepSeconds
                                                , debug
                                                , notice
                                                )


data BrockmanMessage
  = Pinged (IRC.ServerName BS.ByteString)
  | NewFeedItem FeedItem
  | Exception T.Text

handshake :: T.Text -> BotConfig -> ConduitM () IRC.IrcMessage IO ()
handshake nick BotConfig {..} = do
  notice botFeed ("handshake as " <> show nick <> ", joining " <> show botChannels)
  yield $ IRC.Nick $ encodeUtf8 nick
  yield $ IRC.RawMsg $ encodeUtf8 $ "USER " <> nick <> " * 0 :" <> nick
  mapM_ (yield . IRC.Join . encodeUtf8) botChannels
  -- maybe join channels separated by comma

botThread :: TVar (Bloom BS.ByteString) -> T.Text -> BotConfig -> BrockmanConfig -> IO ()
botThread bloom nick bot@BotConfig {..} config@BrockmanConfig {..} = runIRC config $ \chan -> do
  handshake nick bot
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


feedThread :: BotConfig -> Bool -> TVar (Bloom BS.ByteString) -> Chan BrockmanMessage -> IO ()
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


runIRC :: BrockmanConfig -> (Chan BrockmanMessage -> ConduitM () IRC.IrcMessage IO ()) -> IO ()
runIRC BrockmanConfig {..} produce = do
  chan <- newChan
  (if configUseTls == Just True then IRC.ircTLSClient else IRC.ircClient)
    (fromMaybe 6667 $ ircPort configIrc)
    (encodeUtf8 $ ircHost configIrc)
    initialize
    (listenForPing chan)
    (produce chan)
 where
  initialize = pure ()
  listenForPing chan = forever $ do
    maybeMessage <- await
    case maybeMessage of
      Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> do
        debug "" ("Pinged by " <> show s)
        liftIO $ writeChan chan (Pinged s)
      _ -> pure ()

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  debug "" ("Shortening " <> show item <> " with " <> show url)
  r <- post url ["uri" := itemLink item]
  pure item { itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody }
