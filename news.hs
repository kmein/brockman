{-# LANGUAGE DeriveGeneric, FlexibleContexts, LambdaCase, OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, forever)
import qualified Control.Monad.State as State
import Control.Monad.Trans
import Data.Aeson
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom (fromList, insertList, notElem)
import Data.BloomFilter.Hash (cheapHashes)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (readFile, unpack)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text (pack)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import GHC.Generics (Generic)
import Lens.Micro ((&), (^.))
import Network.IRC.Client
import Network.IRC.Conduit
import qualified Network.Wreq as Wreq (get, responseBody)
import System.Environment (getArgs)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as Feed (Feed(..))
import Text.RSS.Syntax (RSSItem(..), rssChannel, rssItems)

data Item = Item
  { ni_title :: Text
  , ni_link :: Text
  } deriving (Show)

type Filter = Bloom BS.ByteString

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
        title = Text.pack $ Atom.txtToString (Atom.entryTitle entry)
        links = map Atom.linkHref (Atom.entryLinks entry)
    rssItemToItems item = map (Item title) links
      where
        title = fromMaybe "untitled" (rssItemTitle item)
        links = maybe [] pure (rssItemLink item)

deduplicate :: TVar Filter -> [Item] -> STM [Item]
deduplicate var items = do
  bloom <- readTVar var
  writeTVar var $ Bloom.insertList (map (Text.encodeUtf8 . ni_link) items) bloom
  return $ filter (flip Bloom.notElem bloom . Text.encodeUtf8 . ni_link) items

data Config = Config
  { c_bots :: [Bot]
  , c_channels :: [ChannelName Text]
  } deriving (Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

data Bot = Bot
  { b_nick :: NickName Text
  , b_feeds :: [String]
  , b_delay :: Int
  } deriving (Generic)

instance FromJSON Bot where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

main :: IO ()
main = do
  let connectionC =
        plainConnection "irc.r" 6667 & set username "news" & set flood 0
  [configFile] <- getArgs
  config <- decode <$> LBS8.readFile configFile
  let bloom0 =
        Bloom.fromList (cheapHashes 17) (2 ^ 10 * 1000) ["" :: BS.ByteString]
  state <- atomically $ newTVar bloom0
  forM_ (maybe [] c_bots config) $ \bot -> do
    let instanceC =
          defaultInstanceConfig (b_nick bot) &
          set channels (maybe [] c_channels config)
    ircState <- newIRCState connectionC instanceC state
    _ <- forkIO $ runClientWith ircState
    forkIO $
      flip runIRCAction ircState $ do
        st <- State.get
        let cs = get channels instanceC
        forever $
          forM_ (b_feeds bot) $ \url -> do
            r <- liftIO $ Wreq.get url
            let f = parseFeedString $ LBS8.unpack $ r ^. Wreq.responseBody
            items <- liftIO $ atomically $ deduplicate st $ feedToItems f
            forM_ items $ \item -> forM_ cs $ \channel -> sendTo channel item
            liftIO $ threadDelay (b_delay bot * 10 ^ 6)
  forever $ threadDelay $ 10 ^ 6
  where
    sendTo channel = send . Privmsg channel . Right . display
    display (Item t l) = t <> " " <> l

