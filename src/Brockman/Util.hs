{-# LANGUAGE TypeApplications #-}

module Brockman.Util where

import Control.Concurrent (killThread, myThreadId, threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8With)
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
