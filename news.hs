{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Prelude hiding (elem)
import Network.Wreq
import Control.Lens
import Text.Feed.Import
import qualified Text.Atom.Feed as Atom
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Text.RSS.Syntax
import qualified Text.Feed.Types as Feed
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Easy as Bloom
import qualified Data.BloomFilter.Hash as Bloom
import Control.Concurrent
import System.Environment
import System.IO
import Control.Exception


data Item = Item
    { ni_title :: BS.ByteString
    , ni_link :: BS.ByteString
    }
  deriving Show


atomEntryToItems :: Atom.Entry -> [Item]
atomEntryToItems entry =
    map (Item title) links
  where
    title = BS8.pack $ Atom.txtToString (Atom.entryTitle entry)
    links = map (BS8.pack . Atom.linkHref) (Atom.entryLinks entry)

rssItemToItems :: RSSItem -> [Item]
rssItemToItems item =
    map (Item title) links
  where
    title = maybe "untitled" BS8.pack (rssItemTitle item)
    links = maybe [] ((:[]) . BS8.pack) (rssItemLink item)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [url, delayString] <- getArgs
    let delay = read delayString :: Int
    let bloom0 = Bloom.fromList (Bloom.cheapHashes 3) 1024 [""]
    rec url delay bloom0
    return ()
  where
    rec url delay bloom =
      ( do
        r <- get url
        let f = parseFeedString $ LBS8.unpack $ r ^. responseBody
            items =
              case f of
                Just (Feed.RSSFeed rss) ->
                    concatMap rssItemToItems (rssItems (rssChannel rss))
                Just (Feed.AtomFeed atom) ->
                    concatMap atomEntryToItems (Atom.feedEntries atom)
                _ -> []

            bloom' = Bloom.insertList (map ni_link items) bloom
            newLinkItems = filter (\item ->
                    let link = ni_link item in
                    link `Bloom.notElem` bloom
                ) items
        mapM_ print (map (\item ->
                let title = ni_title item
                    link = ni_link item
                in title <> " " <> link
            ) newLinkItems)
        threadDelay (delay * 10^6)
        rec url delay bloom'
      )
        `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeException)) >> rec url delay bloom


