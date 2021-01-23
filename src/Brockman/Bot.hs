{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Bot where

import Brockman.Types
import Brockman.Util (notice)
import Control.Concurrent.Chan
import Data.ByteString (ByteString)
import Data.CaseInsensitive (foldedCase)
import Data.Conduit
import Data.Conduit.List (sourceList)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Network.IRC.Conduit as IRC

withIrcConnection :: BrockmanConfig -> (Chan a -> ConduitM (Either ByteString IRC.IrcEvent) Void IO ()) -> (Chan a -> ConduitM () IRC.IrcMessage IO ()) -> IO ()
withIrcConnection BrockmanConfig {configIrc, configUseTls} listen speak = do
  chan <- newChan
  (if configUseTls == Just True then IRC.ircTLSClient else IRC.ircClient)
    (fromMaybe 6667 $ ircPort configIrc)
    (encodeUtf8 $ ircHost configIrc)
    (pure ())
    (listen chan)
    (speak chan)

handshake :: Nick -> [Channel] -> ConduitM () IRC.IrcMessage IO ()
handshake nick channels = do
  notice nick ("handshake, joining " <> show channels)
  yield $ IRC.Nick $ encodeUtf8 $ foldedCase nick
  yield $ IRC.RawMsg $ encodeUtf8 $ "USER " <> foldedCase nick <> " * 0 :" <> foldedCase nick
  mapM_ (yield . IRC.Join . encodeUtf8 . foldedCase) channels

-- maybe join channels separated by comma

broadcastNotice :: Monad m => [Channel] -> T.Text -> ConduitT i (IRC.Message ByteString) m ()
broadcastNotice channels message =
  sourceList
    [ IRC.Notice (encodeUtf8 $ foldedCase channel) $ Right $ encodeUtf8 message
      | channel <- channels
    ]

broadcast :: Monad m => [Channel] -> [T.Text] -> ConduitT i (IRC.Message ByteString) m ()
broadcast channels messages =
  sourceList
    [ IRC.Privmsg (encodeUtf8 $ foldedCase channel) $ Right $ encodeUtf8 message
      | channel <- channels,
        message <- messages
    ]
