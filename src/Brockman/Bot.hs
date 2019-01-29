{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}

module Brockman.Bot
  ( runControllerBot
  , runNewsBot
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forM_, unless, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import Data.Text (Text, isPrefixOf, unpack, unwords)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro
import Lens.Micro.Mtl
import qualified Text.Regex.PCRE.Heavy as Regex
import Network.IRC.Client hiding (get)
import Network.Socket (HostName)
import Network.Wreq (FormParam((:=)), get, post, responseBody)
import Text.Feed.Import (parseFeedString)

import Brockman.Feed
import Brockman.Types
import Brockman.Util (eloop, debug, sleepSeconds)

runControllerBot :: TVar (Bloom BS.ByteString) -> BrockmanConfig -> IO ()
runControllerBot bloom config@BrockmanConfig {..} =
  let connectionC = getConnectionConfig config
      instanceC =
        defaultInstanceConfig (controllerNick configController) &
        channels .~ controllerChannels configController &
        handlers .~ [joinOnWelcome, onPrivateMessage]
   in do
      debug "Starting controller bot"
      eloop $ runClient connectionC instanceC config
  where
    onPrivateMessage =
      EventHandler (matchType _Privmsg) $ \_ (_, m) ->
        case m of
          Left _ -> return ()
          Right message ->
            when (controllerNick configController `isPrefixOf` message) $ do
              liftIO $ debug $ "Controller got a message: " <> show message
              case () of
                ()
                  | [newNick, newFeed] <-
                     parseCommand
                       "add ([A-Za-z][A-Za-z0-9[\\\\\\]^_`{|}-]*) (https?://.*)"
                       message -> do
                    let newBot =
                          BotConfig
                            { botNick = newNick
                            , botFeed = newFeed
                            , botDelay = 1
                            , botChannels = controllerChannels configController
                            }
                    liftIO $ debug $ "Controller spawning new bot: " <> show newBot
                    cBots %= (newBot :)
                    void $ liftIO $ forkIO $ runNewsBot bloom newBot config
                  | otherwise ->
                    forM_ (controllerChannels configController) $ \channel ->
                      send $ Privmsg channel (Right ("no comprendo: " <> message))

parseCommand :: Text -> Text -> [Text]
parseCommand regex s =
  if null result
    then []
    else snd (head result)
  where
    result =
      either (const []) id $
      Regex.scan <$> Regex.compileM (encodeUtf8 regex) [] <*> pure s

runNewsBot :: TVar (Bloom BS.ByteString) -> BotConfig -> BrockmanConfig -> IO ()
runNewsBot bloom botConfig@BotConfig {..} config@BrockmanConfig {configShortener} =
  let connectionC = getConnectionConfig config
      instanceC =
        defaultInstanceConfig botNick & channels .~ botChannels &
        handlers .~ [joinOnWelcome, deafenOnWelcome, broadcastOnJoin, nickMangler]
   in do
      debug $ "Starting news bot: " <> show botConfig
      eloop $ runClient connectionC instanceC ()
  where
    deafenOnWelcome =
      EventHandler (matchNumeric 001) $ \_ _ -> do
        instanceC <- snapshot instanceConfig =<< getIRCState
        liftIO $ debug $ "Deafened bot: " <> show botConfig
        send $ Mode (instanceC ^. nick) False [] ["+D"]
    broadcastOnJoin =
      EventHandler (matchWhen (const True)) $ \_ _ -> loop botChannels True
      where
        display item = Data.Text.unwords [itemTitle item, itemLink item]
        loop cs isFirstTime = do
          liftIO $ debug $ "News bot loop: isFirstTime=" <> show isFirstTime
          r <- liftIO $ get $ unpack botFeed
          liftIO $ debug $ "Requested " <> show botFeed
          items <-
            liftIO $
            atomically $
            deduplicate bloom $
            feedToItems $ parseFeedString $ LBS8.unpack $ r ^. responseBody
          unless isFirstTime $
            forM_ items $ \item -> do
              item' <-
                liftIO $
                if shortenerUse configShortener
                  then item `shortenWith` unpack (shortenerUrl configShortener)
                  else pure item
              forM_ cs $ \channel ->
                send $ Privmsg channel (Right (display item'))
          liftIO $ sleepSeconds botDelay
          loop cs False

getConnectionConfig :: BrockmanConfig -> ConnectionConfig s
getConnectionConfig BrockmanConfig {..}
  | configUseTls = tlsConnection (WithDefaultConfig iHost iPort) & connectionSettings
  | otherwise = plainConnection iHost iPort & connectionSettings
  where
    iHost = encodeUtf8 $ ircHost configIrc
    iPort = fromIntegral $ ircPort configIrc
    connectionSettings = (flood .~ 0) . (logfunc .~ stdoutLogger)

shortenWith :: FeedItem -> HostName -> IO FeedItem
item `shortenWith` url = do
  debug $ "Shortening " <> show item <> " with " <> show url
  r <- post url ["uri" := itemLink item]
  pure item {itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody}
