{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Brockman.Util where

import Brockman.OPML
import Brockman.Types (BotConfig (botFeed), BrockmanConfig (..), Encode (encode), Nick)
import Control.Concurrent (killThread, myThreadId, threadDelay)
import Control.Exception (SomeException, handle)
import Control.Lens
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (all, pack, uncons)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (delete, insert)
import Data.Map (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T (cons, dropWhile, filter, stripPrefix, uncons, words)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Network.Wreq (post, responseBody)
import System.Log.Logger

eloop :: IO a -> IO a
eloop x =
  handle @SomeException
    ( \ex -> do
        warningM "brockman" (show ex)
        sleepSeconds 10
        x
    )
    x

sleepSeconds :: Integer -> IO ()
sleepSeconds n = threadDelay $ fromInteger (n * (^) @Integer @Integer 10 6)

optionally :: Applicative f => (a -> f ()) -> Maybe a -> f ()
optionally = maybe (pure ())

notice :: MonadIO m => Nick -> String -> m ()
notice nick message =
  liftIO $ noticeM "brockman" ("[" <> show nick <> "] " <> message)

debug :: MonadIO m => Nick -> String -> m ()
debug nick message =
  liftIO $ debugM "brockman" ("[" <> show nick <> "] " <> message)

warning :: MonadIO m => Nick -> String -> m ()
warning nick message =
  liftIO $ warningM "brockman" ("[" <> show nick <> "] " <> message)

error' :: MonadIO m => Nick -> String -> m ()
error' nick message =
  liftIO $ errorM "brockman" ("[" <> show nick <> "] " <> message)

suicide :: IO ()
suicide = killThread =<< myThreadId

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With $ \_error _ -> Just '?'

insert :: Ord a => a -> Maybe [a] -> Maybe [a]
insert value list
  | Just values <- list, value `elem` values = list
  | otherwise = case Data.List.insert value <$> list of
    Nothing -> Just [value]
    Just xs -> Just xs

delete :: Ord a => a -> Maybe [a] -> Maybe [a]
delete value list = case Data.List.delete value <$> list of
  Nothing -> Nothing
  Just [] -> Nothing
  Just xs -> Just xs

bsWords :: ByteString -> [ByteString]
bsWords = map encodeUtf8 . T.words . decodeUtf8

-- as defined in https://tools.ietf.org/html/rfc1459#page-9
isValidIrcNick :: Nick -> Bool
isValidIrcNick nick =
  case Data.ByteString.Char8.uncons $ encode nick of
    Nothing -> False
    Just (first, rest) -> isLetter first && Data.ByteString.Char8.all (\c -> isLetter c || isNumber c || isSpecial c) rest
  where
    isLetter c = isAsciiLower c || isAsciiUpper c
    isNumber c = c `elem` ("0123456789" :: String)
    isSpecial c = c `elem` ("-[]\\`^{}_|" :: String) -- '_' and '|' are not in the RFC, but they work

sanitizeNick :: Text -> Text
sanitizeNick nick =
  case T.uncons $ removeProtocol nick of
    Just (first, rest)
      | not (isLetter first) -> sanitizeNick rest
      | otherwise -> T.cons first $ T.filter (\c -> isLetter c || isNumber c || isSpecial c) rest
    Nothing -> "-"
  where
    isLetter c = isAsciiLower c || isAsciiUpper c
    isNumber c = c `elem` ("0123456789" :: String)
    isSpecial c = c `elem` ("-[]\\`^{}_|" :: String) -- '_' and '|' are not in the RFC, but they work
    removeProtocol x = fromMaybe x $ T.stripPrefix "//" $ T.dropWhile (/= ':') x

pasteOpml :: Text -> BrockmanConfig -> IO Text
pasteOpml endpoint config = do
  let opml = feedsToOPML $ map (\(nick, botConfig) -> (botFeed botConfig, nick)) $ toList $ configBots config
  response <- post (Data.Text.unpack endpoint) $ pack $ serializeOPML opml
  return $ decodeUtf8 $ Data.ByteString.Lazy.toStrict $ response ^. responseBody
