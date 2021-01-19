{-# LANGUAGE TypeApplications #-}

module Brockman.Util where

import Control.Concurrent (killThread, myThreadId, threadDelay)
import Control.Exception (SomeException, handle)
import Control.Lens
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (delete, insert)
import Data.Text (Text, all, uncons, unpack)
import Data.Text.Encoding (decodeUtf8With)
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

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay (n * 10 ^ 6)

optionally :: Applicative f => (a -> f ()) -> Maybe a -> f ()
optionally = maybe (pure ())

notice :: MonadIO m => Text -> String -> m ()
notice botFeed message =
  liftIO $ noticeM "brockman" ("[" <> unpack botFeed <> "] " <> message)

debug :: MonadIO m => Text -> String -> m ()
debug botFeed message =
  liftIO $ debugM "brockman" ("[" <> unpack botFeed <> "] " <> message)

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

-- as defined in https://tools.ietf.org/html/rfc1459#page-9
isValidIrcNick :: Text -> Bool
isValidIrcNick nick =
  case Data.Text.uncons nick of
    Nothing -> False
    Just (first, rest) -> isLetter first && Data.Text.all (\c -> isLetter c || isNumber c || isSpecial c) rest
  where
    isLetter c = isAsciiLower c || isAsciiUpper c
    isNumber c = c `elem` "0123456789"
    isSpecial c = c `elem` "-[]\\`^{}_|" -- '_' and '|' are not in the RFC, but they work

pasteJson :: ToJSON a => Text -> a -> IO Text
pasteJson endpoint value = do
  response <- post (Data.Text.unpack endpoint) . encodePretty $ toJSON value
  return $ decodeUtf8 $ Data.ByteString.Lazy.toStrict $ response ^. responseBody
