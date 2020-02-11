{-# LANGUAGE TypeApplications #-}

module Brockman.Util where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( SomeException
                                                , handle
                                                )
import           System.Log.Logger

eloop :: IO a -> IO a
eloop x = handle @SomeException
  (\_ -> do
    warningM "brockman" "Something failed. See you in 10 seconds."
    sleepSeconds 10
    x
  )
  x

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay (n * 10 ^ 6)

optionally :: Applicative f => (a -> f ()) -> Maybe a -> f ()
optionally = maybe (pure ())
