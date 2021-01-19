{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brockman.Feed where

import Control.Applicative (Alternative (..))
import Control.Concurrent.MVar
import Control.Monad (join)
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom (insertList, notElem)
import qualified Data.ByteString as BS (ByteString)
import Data.Fixed
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text, intercalate, lines, pack, strip, unwords)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Time.Clock (UTCTime, diffUTCTime, nominalDiffTimeToSeconds)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)
import Data.Time.RFC822 (parseTimeRFC822)
import Text.Atom.Feed (Entry (entryUpdated))
import qualified Text.Atom.Feed as Atom
import Text.Feed.Query (feedItems)
import qualified Text.Feed.Types as Feed (Feed (..), Item (..))
import Text.HTMLEntity (decode')
import Text.RSS.Syntax (RSSItem (rssItemPubDate))
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

data FeedItem = FeedItem
  { itemTitle :: Text,
    itemLink :: Text
  }
  deriving (Show)

display :: FeedItem -> Text
display item = Data.Text.unwords [strip $ itemLink item, decode' $ Data.Text.intercalate " | " $ Data.Text.lines $ strip $ itemTitle item]

feedEntryUtc :: Feed.Item -> Maybe UTCTime
feedEntryUtc item =
  zonedTimeToUTC
    <$> ( parseTime
            =<< case item of
              Feed.RSSItem item -> rssItemPubDate item
              Feed.AtomItem item -> Just $ entryUpdated item
              _ -> Nothing
        )
  where
    parseTime x = parseTimeRFC822 x <|> parseTimeRFC3339 x

averageDelta :: [UTCTime] -> Maybe Int
averageDelta times = fmap (`div` (10 ^ 12)) $ mean $ map (unFixed . nominalDiffTimeToSeconds) $ zipWith diffUTCTime times' (tail times')
  where
    times' = take 10 $ reverse $ sort times
    unFixed (MkFixed x) = x
    -- fromPico x = unFixed x `div` (10^resolution x)
    mean xs
      | null xs = Nothing
      | otherwise = Just $ round $ fromIntegral (sum xs) / fromIntegral (length xs)

feedEntryDelta :: Feed.Feed -> Maybe Int
feedEntryDelta = averageDelta . mapMaybe feedEntryUtc . feedItems

feedToItems :: Maybe Feed.Feed -> [FeedItem]
feedToItems = maybe [] (mapMaybe fromItem . feedItems)
  where
    fromItem = \case
      Feed.AtomItem entry ->
        let title = pack $ Atom.txtToString $ Atom.entryTitle entry
            link = Atom.linkHref <$> listToMaybe (Atom.entryLinks entry)
         in FeedItem title <$> link
      Feed.RSSItem item ->
        let title = fromMaybe "untitled" (RSS.rssItemTitle item)
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
