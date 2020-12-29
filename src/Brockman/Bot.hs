{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Brockman.Bot
  ( botThread
  )
where

import           Data.Conduit
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad                  ( forM_
                                                , unless
                                                , forever
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
import           Text.Feed.Import               ( parseFeedString )

import           Brockman.Feed
import           Brockman.Types
import           Brockman.Util                  ( sleepSeconds
                                                , optionally
                                                )

handshake :: Text -> BotConfig -> ConduitM () IRC.IrcMessage IO ()
handshake nick BotConfig {..} = do
  liftIO $ noticeM "brockman.handshake" ("Handshake of " <> show nick <> ", joining " <> show botChannels)
  yield $ IRC.Nick $ encodeUtf8 nick
  yield $ IRC.RawMsg $ encodeUtf8 $ "USER " <> nick <> " * 0 :" <> nick
  mapM_ (yield . IRC.Join . encodeUtf8) botChannels
  -- maybe join channels separated by comma

botThread :: TVar (Bloom BS.ByteString) -> Text -> BotConfig -> BrockmanConfig -> IO ()
botThread bloom nick bot@BotConfig {..} config@BrockmanConfig {..} = runIRC config $ \pingedMVar -> do
  handshake nick bot
  feedMVar <- liftIO newEmptyMVar
  liftIO $ forkIO $ feedThread bot True bloom feedMVar
  sendNews botChannels pingedMVar feedMVar
 where
  display item = Data.Text.unwords [itemTitle item, itemLink item]
  sendNews :: [Text] -> MVar (IRC.ServerName BS.ByteString) -> MVar [FeedItem] -> ConduitM () IRC.IrcMessage IO ()
  sendNews cs pingedMVar feedMVar = do
    maybeServerName <- liftIO $ tryTakeMVar pingedMVar
    case maybeServerName of
      Just serverName -> do
        liftIO $ debugM "brockman.botThread.sendNews" ("Pong " <> show serverName)
        yield $ IRC.Pong serverName
      Nothing -> pure ()
    maybeFeedItems <- liftIO $ tryTakeMVar feedMVar
    case maybeFeedItems of
      Just items ->
        forM_ items $ \item -> do
          liftIO $ noticeM "brockman.botThread.sendNews" ("Sending " <> show (display item))
          item' <- liftIO $ maybe (pure item) (\url -> item `shortenWith` unpack url) configShortener
          forM_ cs $ \channel -> yield $ IRC.Privmsg (encodeUtf8 channel) $ Right $ encodeUtf8 $ display item'
      Nothing -> pure ()
    liftIO $ sleepSeconds 1 -- dont heat the room
    sendNews cs pingedMVar feedMVar

feedThread :: BotConfig -> Bool -> TVar (Bloom BS.ByteString) -> MVar [FeedItem] -> IO ()
feedThread bot@BotConfig {..} isFirstTime bloom feedMVar = do
    r <- liftIO $ get $ unpack botFeed
    liftIO $ debugM "brockman.botThread" $ show botFeed
    items <-
      liftIO
      $  atomically
      $  deduplicate bloom
      $  feedToItems
      $  parseFeedString
      $  LBS8.unpack
      $  r
      ^. responseBody
    unless isFirstTime $ putMVar feedMVar items
    liftIO $ sleepSeconds (fromMaybe 300 botDelay)
    liftIO $ noticeM "brockman.botThread.sendNews" "finished sleeping"
    feedThread bot False bloom feedMVar


runIRC :: BrockmanConfig -> (MVar (IRC.ServerName BS.ByteString) -> ConduitM () IRC.IrcMessage IO ()) -> IO ()
runIRC BrockmanConfig {..} produce = do
  mvar <- newEmptyMVar
  (if fromMaybe False configUseTls then IRC.ircTLSClient else IRC.ircClient)
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
