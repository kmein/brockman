{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Bot where

import Brockman.Types
import Brockman.Util (notice)
import Control.Concurrent.Chan
import Data.ByteString (ByteString)
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
  yield $ IRC.Nick $ encode nick
  yield $ IRC.RawMsg $ "USER " <> encode nick <> " * 0 :" <> encode nick
  mapM_ (yield . IRC.Join . encode) channels

deafen :: Nick -> ConduitM () IRC.IrcMessage IO ()
deafen nick = yield $ IRC.Mode (encode nick) False [] ["+D"] -- deafen to PRIVMSGs

-- maybe join channels separated by comma

broadcastNotice :: Monad m => [Channel] -> T.Text -> ConduitT i (IRC.Message ByteString) m ()
broadcastNotice channels message =
  sourceList
    [ IRC.Notice (encode channel) $ Right $ encodeUtf8 message
      | channel <- channels
    ]

broadcast :: Monad m => [Channel] -> [T.Text] -> ConduitT i (IRC.Message ByteString) m ()
broadcast channels messages =
  sourceList
    [ IRC.Privmsg (encode channel) $ Right $ encodeUtf8 message
      | channel <- channels,
        message <- messages
    ]
