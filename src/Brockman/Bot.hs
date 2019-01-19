{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Brockman.Bot where

import Control.Concurrent.STM
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.BloomFilter (Bloom)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import Data.Text (unpack)
import qualified Data.Text as Text (unwords)
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro
import Network.IRC.Client
import qualified Network.Wreq as Wreq (FormParam((:=)), get, post, responseBody)
import Text.Feed.Import (parseFeedString)

import Brockman.Feed
import Brockman.Types
import Brockman.Util (sleepSeconds)

botThread ::
     TVar (Bloom BS.ByteString)
  -> NewsBot
  -> BotsConfig
  -> BrockmanOptions
  -> IO ()
botThread bloom NewsBot{..} BotsConfig{..} BrockmanOptions {..} =
  let connectionC
        | useTLS = tlsConnection (WithDefaultConfig (BS8.pack ircHost) (fromIntegral ircPort)) & connectionSettings
        | otherwise = plainConnection (BS8.pack ircHost) (fromIntegral ircPort) & connectionSettings
        where connectionSettings = (flood .~ 0) . (logfunc .~ stdoutLogger)
      instanceC =
        defaultInstanceConfig botNick &
        channels .~ configChannels &
        handlers .~
        [joinOnWelcome, pingHandler, deafenOnWelcome, broadcastOnJoin]
   in runClient connectionC instanceC ()
  where
    deafenOnWelcome =
      EventHandler (matchNumeric 001) $ \_ _ -> do
        instanceC <- snapshot instanceConfig =<< getIRCState
        send $ Mode (instanceC ^. nick) False [] ["+D"]
    broadcastOnJoin =
      EventHandler (matchWhen (const True)) $ \_ _ -> do
        instanceC <- snapshot instanceConfig =<< getIRCState
        forever $
          forM_ botFeeds $ \url -> do
            r <- liftIO $ Wreq.get $ unpack url
            let f = parseFeedString $ LBS8.unpack $ r ^. Wreq.responseBody
            items <- liftIO $ atomically $ deduplicate bloom $ feedToItems f
            forM_ items $ \item -> do
              item' <-
                liftIO $
                (if shorten
                   then goify
                   else pure)
                  item
              forM_ (instanceC ^. channels) $ \channel ->
                send $ Privmsg channel (Right (display item'))
            liftIO $ sleepSeconds botDelay
      where
        display item = Text.unwords [itemTitle item, itemLink item]

goify :: FeedItem -> IO FeedItem
goify item = do
  r <- Wreq.post "http://go.lassul.us" ["uri" Wreq.:= itemLink item]
  pure item {itemLink = decodeUtf8 $ BL.toStrict $ r ^. Wreq.responseBody}
