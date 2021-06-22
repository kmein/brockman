{-# LANGUAGE TupleSections #-}

module Brockman.OPML (opmlToFeeds, feedsToOPML, parseOPMLString, serializeOPML) where

import Brockman.Types (Nick (..), URL)
import Data.CaseInsensitive (foldedCase)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, unpack)
import Text.OPML.Reader
import Text.OPML.Syntax
import Text.OPML.Writer
import Text.XML.Light.Types

opmlToFeeds :: OPML -> [(URL, Maybe Text)]
opmlToFeeds = mapMaybe outlineToFeed . opmlOutlines
  where
    opmlOutlines :: OPML -> [Outline]
    opmlOutlines = concatMap flattenTree . opmlBody
      where
        flattenTree outline = outline : concatMap flattenTree (opmlOutlineChildren outline)
    outlineToFeed :: Outline -> Maybe (URL, Maybe Text)
    outlineToFeed outline = (,findAttr "title") <$> findAttr "xmlUrl"
      where
        findAttr key = pack . attrVal <$> find (\attr -> qName (attrKey attr) == key) (opmlOutlineAttrs outline)

feedsToOPML :: [(URL, Nick)] -> OPML
feedsToOPML feeds =
  nullOPML
    { opmlBody =
        map
          ( \(feedUrl, feedNick) ->
              (nullOutline "")
                { opmlOutlineAttrs =
                    [ Attr (QName "type" Nothing Nothing) "rss",
                      Attr (QName "title" Nothing Nothing) (unpack $ foldedCase $ unNick feedNick),
                      Attr (QName "xmlUrl" Nothing Nothing) (unpack feedUrl)
                    ]
                }
          )
          feeds
    }
