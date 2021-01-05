{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

module Brockman.Bot
  ( botThread
  )
where

import           Data.Conduit
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar
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
import qualified Data.ByteString.Lazy.Char8    as LBS8
                                                ( unpack )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , unpack
                                                , unwords
                                                )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Lens.Micro
import qualified Network.IRC.Conduit           as IRC
import           Network.Socket                 ( HostName )
import           Network.Wreq                   ( FormParam((:=))
                                                , get
                                                , post
                                                , responseBody
                                                )
import           System.Log.Logger
import           System.Random                  ( randomRIO )
import           Text.Feed.Import               ( parseFeedSource )

import           Brockman.Feed
import           Brockman.Types
import           Brockman.Util                  ( sleepSeconds
                                                , optionally
                                                )

handshake :: Text -> BotConfig -> ConduitM () IRC.IrcMessage IO ()
handshake nick bot@BotConfig {..} = do
  liftIO $ noticeM "brockman.handshake" ("[" <> unpack botFeed <> "] Handshake as " <> show nick <> ", joining " <> show botChannels)
  yield $ IRC.Nick $ encodeUtf8 nick
  yield $ IRC.RawMsg $ encodeUtf8 $ "USER " <> nick <> " * 0 :" <> nick
  mapM_ (yield . IRC.Join . encodeUtf8) botChannels
  -- maybe join channels separated by comma

botThread :: TVar (Bloom BS.ByteString) -> Text -> BotConfig -> BrockmanConfig -> IO ()
botThread bloom nick bot@BotConfig {..} config@BrockmanConfig {..} = runIRC config $ \pingedMVar -> do
  handshake nick bot
  feedMVar <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ feedThread bot True bloom feedMVar
  sendNews botChannels pingedMVar feedMVar
 where
  display item = Data.Text.unwords [itemTitle item, itemLink item]
  sendNews :: [Text] -> MVar (IRC.ServerName BS.ByteString) -> MVar [FeedItem] -> ConduitM () IRC.IrcMessage IO ()
  sendNews cs pingedMVar feedMVar =
    forever $ do
      liftIO (tryTakeMVar pingedMVar) >>= \case
        Nothing -> pure ()
        Just serverName -> do
          liftIO $ debugM "brockman.botThread.sendNews" ("[" <> unpack botFeed <> "] Pong " <> show serverName)
          yield $ IRC.Pong serverName
      liftIO (tryTakeMVar feedMVar) >>= \case
        Nothing -> pure ()
        Just items ->
          forM_ items $ \item -> do
            liftIO $ noticeM "brockman.botThread.sendNews" ("[" <> unpack botFeed <> "] Sending " <> show (display item))
            item' <- liftIO $ maybe (pure item) (\url -> item `shortenWith` unpack url) configShortener
            forM_ cs $ \channel -> yield $ IRC.Privmsg (encodeUtf8 channel) $ Right $ encodeUtf8 $ display item'
      liftIO $ sleepSeconds 1 -- dont heat the room

feedThread :: BotConfig -> Bool -> TVar (Bloom BS.ByteString) -> MVar [FeedItem] -> IO ()
feedThread bot@BotConfig {..} isFirstTime bloom feedMVar = do
    let delaySeconds = fromMaybe 300 botDelay
    liftIO $ when isFirstTime $ do
      randomDelay <- randomRIO (0, delaySeconds)
      debugM "brockman.feedThread" $ "[" <> unpack botFeed <> "] sleep " <> show randomDelay
      sleepSeconds randomDelay
    r <- E.try $ get $ unpack botFeed
    liftIO $ debugM "brockman.feedThread" $ "[" <> unpack botFeed <> "] fetch"
    case r of
      Left (E.SomeException ex) -> do  -- TODO handle exceptions, print to irc
        liftIO $ debugM "brockman.feedThread" $ "[" <> unpack botFeed <> "] exception" <> show ex
      Right resp -> do
        items <-
          liftIO
          $  atomically
          $  deduplicate bloom
          $  feedToItems
          $  parseFeedSource
          $  resp
          ^. responseBody
        unless isFirstTime $ putMVar feedMVar items
    liftIO $ sleepSeconds delaySeconds
    liftIO $ debugM "brockman.feedThread" ("[" <> unpack botFeed <> "] tick")
    feedThread bot False bloom feedMVar


runIRC :: BrockmanConfig -> (MVar (IRC.ServerName BS.ByteString) -> ConduitM () IRC.IrcMessage IO ()) -> IO ()
runIRC BrockmanConfig {..} produce = do
  mvar <- newEmptyMVar
  (if configUseTls == Just True then IRC.ircTLSClient else IRC.ircClient)
    (fromMaybe 6667 $ ircPort configIrc)
    (encodeUtf8 $ ircHost configIrc)
    initialize
    (consumeIrc mvar)
    (produce mvar)
 where
  initialize = pure ()
  consumeIrc mvar = forever $ do
    maybeMessage <- await
    optionally (liftIO . debugM "brockman.runIRC" . show) maybeMessage
    case maybeMessage of
      Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ putMVar mvar s
      _ -> pure ()

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  infoM "brockman.shortenWith" ("Shortening " <> show item <> " with " <> show url)
  r <- post url ["uri" := itemLink item]
  pure item { itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody }
