{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase,
  OverloadedStrings, ScopedTypeVariables,
  ViewPatterns #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom (fromList, insertList, notElem)
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile, unpack)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text (unwords)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Text.IO (hPutStrLn)
import GHC.Generics (Generic)
import Kirk.Config
import Kirk.Simple
import Lens.Micro ((^.))
import qualified Network.Wreq as Wreq (get, responseBody)
import Safe (readMay)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as Feed (Feed(..))
import Text.RSS.Syntax (RSSItem(..), rssChannel, rssItems)

import Util

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

data BrockmanConfig = BrockmanConfig
  { c_bots :: [NewsBot]
  , c_channels :: [Text]
  } deriving (Generic)

instance FromJSON BrockmanConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

data NewsBot = NewsBot
  { b_nick :: Text
  , b_feeds :: [Text]
  , b_delay :: Int
  } deriving (Generic)

instance FromJSON NewsBot where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

botThread :: TVar (Bloom BS.ByteString) -> NewsBot -> Config -> IO ()
botThread bloom bot botConfig =
  run botConfig $ \h -> do
    handshake botConfig h
    race_ (ircAgent botConfig h) $ do
      hPutStrLn h $ "MODE " <> b_nick bot <> " +D"
      eloop $
        forever $
        forM_ (b_feeds bot) $ \url -> do
          r <- Wreq.get $ unpack url
          let f = parseFeedString $ LBS8.unpack $ r ^. Wreq.responseBody
          items <- atomically $ deduplicate bloom $ feedToItems f
          forM_ items $ privmsg botConfig h . display
          sleepSeconds (b_delay bot)
  where
    display (Item t l) = Text.unwords [t, l]

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [ircHost, readMay -> Just ircPort, configFile] -> do
      config <- decode <$> LBS8.readFile configFile
      let bloom0 = Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) [""]
      bloom <- atomically $ newTVar bloom0
      forConcurrently_ (maybe [] c_bots config) $ \bot ->
        eloop $
        botThread
          bloom
          bot
          Config
            { nick = b_nick bot
            , msgtarget = maybe [] c_channels config
            , server_hostname = ircHost
            , server_port = ircPort
            }
      forever $ sleepSeconds 1
    _ -> do
      programName <- pack <$> getProgName
      hPutStrLn stderr $
        "Usage: " <> programName <> " IRC-SERVER IRC-PORT CONFIG"
      exitFailure
