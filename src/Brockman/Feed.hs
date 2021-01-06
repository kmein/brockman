{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Feed where

import Control.Concurrent.MVar
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom (insertList, notElem)
import qualified Data.ByteString as BS (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, strip, unwords)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Types as Feed (Feed (..))
import Text.RSS.Syntax (RSSItem (..), rssChannel, rssItems)

data FeedItem = FeedItem
  { itemTitle :: Text,
    itemLink :: Text
  }
  deriving (Show)

display :: FeedItem -> Text
display item = Data.Text.unwords [strip $ itemLink item, strip $ itemTitle item]

feedToItems :: Maybe Feed.Feed -> [FeedItem]
feedToItems = \case
  Just (Feed.RSSFeed rss) ->
    concatMap rssItemToItems (rssItems (rssChannel rss))
  Just (Feed.AtomFeed atom) ->
    concatMap atomEntryToItems (Atom.feedEntries atom)
  _ -> []
  where
    atomEntryToItems entry = map (FeedItem title) links
      where
        title = pack $ Atom.txtToString $ Atom.entryTitle entry
        links = map Atom.linkHref (Atom.entryLinks entry)
    rssItemToItems item = map (FeedItem title) links
      where
        title = fromMaybe "untitled" (rssItemTitle item)
        links = maybe [] pure (rssItemLink item)

deduplicate :: MVar (Bloom BS.ByteString) -> [FeedItem] -> IO [FeedItem]
deduplicate var items =
  modifyMVar var $ \bloom ->
    let bloom' = Bloom.insertList (map (Text.encodeUtf8 . itemLink) items) bloom
        items' = filter (flip Bloom.notElem bloom . Text.encodeUtf8 . itemLink) items
     in pure (bloom', items')
