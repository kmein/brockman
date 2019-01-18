{-# LANGUAGE TypeApplications #-}

module Brockman.Util where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)

eloop :: IO a -> IO a
eloop = handle @SomeException =<< const

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay (n * 10 ^ 6)
