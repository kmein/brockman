{-# LANGUAGE TypeApplications #-}

module Brockman.Util where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , handle
                                                )
import           System.Log.Logger

eloop :: IO a -> IO a
eloop x = handle @SomeException
  (\ex -> do
    warningM "brockman" (show ex)
    sleepSeconds 10
    x
  )
  x

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay (n * 10 ^ 6)

optionally :: Applicative f => (a -> f ()) -> Maybe a -> f ()
optionally = maybe (pure ())
