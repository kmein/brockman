{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Brockman.Feed where

import Control.Applicative (Alternative (..))
import Data.Fixed
import Data.List
import qualified Data.Cache.LRU as LRU
import Data.Hashable (hash)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text, intercalate, lines, pack, strip, unwords)
import Data.Time.Clock
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

type LRU = LRU.LRU Int ()

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
              Feed.RSSItem rssItem -> rssItemPubDate rssItem
              Feed.AtomItem atomItem -> Just $ entryUpdated atomItem
              _ -> Nothing
        )
  where
    parseTime x = parseTimeRFC822 x <|> parseTimeRFC3339 x

averageDelta :: [UTCTime] -> Maybe Integer
averageDelta times = fmap (`div` pico) $ mean $ map (unFixed . nominalDiffTimeToSeconds) $ zipWith diffUTCTime times' (tail times')
  where
    pico = (^) @Integer @Integer 10 12
    times' = take 10 $ reverse $ nubBy sameMinute $ sort times
    sameMinute time1 time2 = abs (diffUTCTime time1 time2) < secondsToNominalDiffTime 60
    unFixed (MkFixed x) = x
    mean xs
      | null xs = Nothing
      | otherwise = Just $ round @Double $ fromIntegral (sum xs) / fromIntegral (length xs)

feedEntryDelta :: UTCTime -> Feed.Feed -> Maybe Integer
feedEntryDelta now = averageDelta . (:) now . mapMaybe feedEntryUtc . feedItems

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

deduplicate :: Maybe LRU -> [FeedItem] -> (LRU, [FeedItem])
deduplicate maybeLRU items =
  case maybeLRU of
    Nothing ->
      (freshLru, [])
    Just lru ->
      case (LRU.maxSize lru) of
        Just size -> if size < genericLength items then
            (freshLru, [])
          else
            foldl' step (lru, []) items
        Nothing -> foldl' step (lru, []) items
  where
    freshLru = LRU.fromList (Just $ genericLength items * 2) $ map (\i -> (key i, ())) items
    key = hash . itemLink
    step (lru, items') item =
      let (newLru, maybeItem) = LRU.lookup (key item) lru
      in case maybeItem of
        Nothing -> (LRU.insert (key item) () newLru, item : items')
        Just () -> (newLru, items')
