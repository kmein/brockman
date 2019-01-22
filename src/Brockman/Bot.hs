{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Brockman.Bot
  ( runControllerBot
  , runNewsBot
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import Data.Text (unpack, unwords)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Lens.Micro
import Lens.Micro.Mtl
import Network.IRC.Client hiding (get)
import Network.Socket (HostName)
import Network.Wreq (FormParam((:=)), get, post, responseBody)
import Text.Feed.Import (parseFeedString)

import Brockman.Feed
import Brockman.Types
import Brockman.Util (sleepSeconds)

runControllerBot :: BrockmanConfig -> IO ()
runControllerBot config@BrockmanConfig {..} =
  let connectionC = getConnectionConfig config
      instanceC =
        defaultInstanceConfig configController & channels .~ configChannels &
        handlers .~ []
   in runClient connectionC instanceC config

runNewsBot :: TVar (Bloom BS.ByteString) -> BotConfig -> BrockmanConfig -> IO ()
runNewsBot bloom BotConfig {..} config@BrockmanConfig {..} =
  let connectionC = getConnectionConfig config
      instanceC =
        defaultInstanceConfig botNick & channels .~ configChannels &
        handlers .~
        [joinOnWelcome, pingHandler, deafenOnWelcome, broadcastOnJoin]
   in runClient connectionC instanceC ()
  where
    deafenOnWelcome =
      EventHandler (matchNumeric 001) $ \_ _ -> do
        instanceC <- snapshot instanceConfig =<< getIRCState
        send $ Mode (instanceC ^. nick) False [] ["+D"]
    broadcastOnJoin =
      EventHandler (matchWhen (const True)) $ \_ _ -> loop configChannels True
      where
        display item = Data.Text.unwords [itemTitle item, itemLink item]
        loop cs isFirstTime =
          forM_ botFeeds $ \url -> do
            r <- liftIO $ get $ unpack url
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
  r <- post url ["uri" := itemLink item]
  pure item {itemLink = decodeUtf8 $ BL.toStrict $ r ^. responseBody}
