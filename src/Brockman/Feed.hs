{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Feed where

import Control.Concurrent.MVar
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom (insertList, notElem)
import qualified Data.ByteString as BS (ByteString)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text, pack, strip, unwords, lines, intercalate)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Query (feedItems)
import qualified Text.Feed.Types as Feed (Feed (..), Item(..))
import Text.HTMLEntity (decode')
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

data FeedItem = FeedItem
  { itemTitle :: Text,
    itemLink :: Text
  }
  deriving (Show)

display :: FeedItem -> Text
display item = Data.Text.unwords [strip $ itemLink item, decode' $ Data.Text.intercalate " | " $ Data.Text.lines $ strip $ itemTitle item]

feedToItems :: Maybe Feed.Feed -> [FeedItem]
feedToItems = maybe [] (mapMaybe fromItem . feedItems)
  where
    fromItem = \case
      Feed.AtomItem entry ->
        let
          title = pack $ Atom.txtToString $ Atom.entryTitle entry
          link = Atom.linkHref <$> listToMaybe (Atom.entryLinks entry)
         in FeedItem title <$> link
      Feed.RSSItem item ->
        let
          title = fromMaybe "untitled" (RSS.rssItemTitle item)
          link = RSS.rssItemLink item
         in FeedItem title <$> link
      Feed.RSS1Item item ->
        Just $ FeedItem (RSS1.itemTitle item) (RSS1.itemLink item)
      _ -> Nothing

deduplicate :: MVar (Bloom BS.ByteString) -> [FeedItem] -> IO [FeedItem]
deduplicate var items =
  modifyMVar var $ \bloom ->
    let bloom' = Bloom.insertList (map (Text.encodeUtf8 . itemLink) items) bloom
        items' = filter (flip Bloom.notElem bloom . Text.encodeUtf8 . itemLink) items
     in pure (bloom', items')
