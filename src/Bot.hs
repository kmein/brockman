{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Bot where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.BloomFilter (Bloom)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import Data.Text (Text, unpack)
import qualified Data.Text as Text (unwords)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Kirk.Config
import Kirk.Simple
import Lens.Micro ((^.))
import qualified Network.Wreq as Wreq (get, post, responseBody)
import Text.Feed.Import (parseFeedString)

import Feed
import Util (eloop, sleepSeconds)

data NewsBot = NewsBot
  { b_nick :: Text
  , b_feeds :: [Text]
  , b_delay :: Int
  } deriving (Generic)

instance FromJSON NewsBot where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

botThread ::
     TVar (Bloom BS.ByteString) -> NewsBot -> Maybe String -> Config -> IO ()
botThread bloom bot shortener botConfig =
  run botConfig $ \h -> do
    handshake botConfig h
    race_ (ircAgent botConfig h) $ do
      mode botConfig h ["+D"]
      eloop $
        forever $
        forM_ (b_feeds bot) $ \url -> do
          r <- Wreq.get $ unpack url
          let f = parseFeedString $ LBS8.unpack $ r ^. Wreq.responseBody
          items <- atomically $ deduplicate bloom $ feedToItems f
          forM_ items $ shorten shortener >=> (privmsg botConfig h . display)
          sleepSeconds (b_delay bot)
  where
    display item = Text.unwords [fi_title item, fi_link item]

shorten :: Maybe String -> FeedItem -> IO FeedItem
shorten Nothing item = pure item
shorten (Just url) item = do
  r <- Wreq.post url (encodeUtf8 $ fi_link item)
  pure item { fi_link = decodeUtf8 $ BL.toStrict $ r ^. Wreq.responseBody }
