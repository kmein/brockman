{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

module Brockman.Bot
  ( botThread
  )
where

import           Data.Conduit
import           Control.Concurrent.STM
import           Control.Concurrent.MVar
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
import           Data.IORef
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
import           Brockman.Util                  ( eloop
                                                , sleepSeconds
                                                )

handshake :: BotConfig -> ConduitM () IRC.IrcMessage IO ()
handshake BotConfig {..} = do
  yield $ IRC.Nick $ encodeUtf8 botNick
  yield $ IRC.RawMsg $ encodeUtf8 $ "USER " <> botNick <> " * 0 :" <> botNick
  mapM_ (yield . IRC.Join . encodeUtf8) botChannels
  -- maybe join channels separated by comma

botThread :: TVar (Bloom BS.ByteString) -> BotConfig -> BrockmanConfig -> IO ()
botThread bloom bot@BotConfig {..} config@BrockmanConfig {..} =
  runIRC config $ \mvar -> do
    handshake bot
    yield $ IRC.Mode (encodeUtf8 botNick) True ["D"] []
    sendNews botChannels True mvar
 where
  display item = Data.Text.unwords [itemTitle item, itemLink item]
  sendNews
    :: [Text]
    -> Bool
    -> MVar (IRC.ServerName BS.ByteString)
    -> ConduitM () IRC.IrcMessage IO ()
  sendNews cs isFirstTime mvar = do
    maybeServerName <- liftIO $ tryTakeMVar mvar
    maybe (pure ()) (yield . IRC.Pong) maybeServerName
    r <- liftIO $ eloop $ get $ unpack botFeed
    liftIO $ infoM "brockman.botThread" $ "Fetched " <> show botFeed
    items <-
      liftIO
      $  atomically
      $  deduplicate bloom
      $  feedToItems
      $  parseFeedString
      $  LBS8.unpack
      $  r
      ^. responseBody
    unless isFirstTime $ forM_ items $ \item -> do
      item' <- liftIO $ if shortenerUse configShortener
        then item `shortenWith` unpack (shortenerUrl configShortener)
        else pure item
      forM_ cs $ \channel ->
        yield $ IRC.Privmsg (encodeUtf8 channel) $ Right $ encodeUtf8 $ display
          item'
    liftIO $ sleepSeconds botDelay
    sendNews cs False mvar


runIRC
  :: BrockmanConfig
  -> (  MVar (IRC.ServerName BS.ByteString)
     -> ConduitM () IRC.IrcMessage IO ()
     )
  -> IO ()
runIRC BrockmanConfig {..} produce = do
  mvar <- newEmptyMVar
  (if configUseTls then IRC.ircTLSClient else IRC.ircClient)
    (ircPort configIrc)
    (encodeUtf8 $ ircHost configIrc)
    initialize
    (consume mvar)
    (produce mvar)
 where
  logMessage = \case
    Just (Right (IRC.Event _ src msg)) -> liftIO $ infoM
      "brockman.runIRC"
      ("Got an event: " <> show src <> " - " <> show msg)
    Just (Left raw) ->
      liftIO $ infoM "brockman.runIRC" ("Got raw bytes: " <> show raw)
    _ -> pure ()
  initialize = pure ()
  consume mvar = forever $ do
    maybeMessage <- await
    logMessage maybeMessage
    case maybeMessage of
      Just (Right (IRC.Event _ _ (IRC.Ping s _))) -> liftIO $ putMVar mvar s
      _ -> pure ()

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  infoM "brockman.shortenWith"
        ("Shortening " <> show item <> " with " <> show url)
  r <- post url ["uri" := itemLink item]
  pure item { itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody }
