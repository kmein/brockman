{-# LANGUAGE TypeApplications #-}
module Util where

import Control.Exception (SomeException, handle)
import Control.Concurrent (threadDelay)

eloop :: IO a -> IO a
eloop = handle @SomeException =<< const

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay (n * 10 ^ 6)

