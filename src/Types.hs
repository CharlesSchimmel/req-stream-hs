{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Either
import Data.HashMap.Strict
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import Debug.Trace
import Network.HTTP.Req
import Streaming.Prelude as S hiding (mapM, mconcat, show, split)

data RedditTimeEnum
  = Hour
  | Day
  | Week
  | Month
  | Year
  | All
  deriving (Show, Enum)

instance RedditTime RedditTimeEnum where
  fromRedditTime = toLower . pack . show

data RedditOrder rTime
  = Top RedditTimeEnum
  | Hot
  | New
  | Best
  deriving (Show)

time :: RedditTime t => t -> Option s
time = ("t" =:) . fromRedditTime

class RedditTime t where
  fromRedditTime :: t -> Text

data Listing = Listing
  { listingData :: Data
  } deriving (Show)

instance FromJSON Listing where
  parseJSON =
    withObject "Listing" $ \o -> do
      foundKind <- (== "Listing") <$> (o .: "kind" :: Parser String)
      let rightKind = Listing <$> (o .: "data" :: Parser Data)
      if foundKind
        then rightKind
        else fail "Did not find kind 'Listing'"

type Fullname = String

data Data = Data
  { modhash :: Text
  , dist :: Integer
  , after :: Maybe Fullname
  , before :: Maybe Fullname
  , children :: V.Vector Link
  } deriving (Show)

instance FromJSON Data where
  parseJSON =
    withObject "Data" $ \o ->
      Data <$> o .: "modhash" <*> o .: "dist" <*> o .:? "after" <*>
      o .:? "before" <*>
      o .: "children"

data Link = Link
  { url :: Text
  , title :: Text
  , author :: Text
  , name :: Fullname -- a sorta guid for reddit items
  , createdUtc :: Integer -- epoch time
  } deriving (Show)

-- should check link kind as well
instance FromJSON Link where
  parseJSON =
    withObject "Link" $ \o -> do
      oData <- o .: "data"
      url <- oData .: "url"
      title <- oData .: "title"
      author <- oData .: "author"
      name <- oData .: "name"
      createdUtc <- oData .: "created_utc"
      return Link {..}
