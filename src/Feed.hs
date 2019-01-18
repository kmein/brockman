{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Feed where

import Data.Text (Text, pack)
import qualified Text.Atom.Feed as Atom
import Data.Maybe (fromMaybe)
import qualified Text.Feed.Types as Feed (Feed(..))
import Text.RSS.Syntax (RSSItem(..), rssChannel, rssItems)
import Data.BloomFilter (Bloom)
import Control.Concurrent.STM
import qualified Data.ByteString as BS (ByteString)
import qualified Data.BloomFilter as Bloom (insertList, notElem)
import qualified Data.Text.Encoding as Text (encodeUtf8)

data Item = Item
  { ni_title :: Text
  , ni_link :: Text
  } deriving (Show)

feedToItems :: Maybe Feed.Feed -> [Item]
feedToItems =
  \case
    Just (Feed.RSSFeed rss) ->
      concatMap rssItemToItems (rssItems (rssChannel rss))
    Just (Feed.AtomFeed atom) ->
      concatMap atomEntryToItems (Atom.feedEntries atom)
    _ -> []
  where
    atomEntryToItems entry = map (Item title) links
      where
        title = pack $ Atom.txtToString $ Atom.entryTitle entry
        links = map Atom.linkHref (Atom.entryLinks entry)
    rssItemToItems item = map (Item title) links
      where
        title = fromMaybe "untitled" (rssItemTitle item)
        links = maybe [] pure (rssItemLink item)

deduplicate :: TVar (Bloom BS.ByteString) -> [Item] -> STM [Item]
deduplicate var items = do
  bloom <- readTVar var
  writeTVar var $ Bloom.insertList (map (Text.encodeUtf8 . ni_link) items) bloom
  return $ filter (flip Bloom.notElem bloom . Text.encodeUtf8 . ni_link) items

