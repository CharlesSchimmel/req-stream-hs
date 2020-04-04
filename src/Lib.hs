{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Auth
import Types

import Stream

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

someFunc :: IO ()
someFunc = runReq defaultHttpConfig downTopTenImages

topImages :: Req (Either String (V.Vector Link))
topImages =
  fmap (children . listingData) . parseEither parseJSON . responseBody <$>
  req GET url_ NoReqBody jsonResponse options
  where
    url_ = https "reddit.com" /: "r" /: "earthporn" /: "top.json"
    options :: Option Https
    options =
      mconcat
        [ header "Accept" "application/json"
        , header "User-Agent" "haskell-reddit-test"
        , basicAuthHeaders
        , limit 20
        , time Day
        ]

newposts :: Text -> Req (Either String Listing)
newposts subredditName =
  parseEither parseJSON . responseBody <$>
  req GET url_ NoReqBody jsonResponse options
  where
    url_ = https "reddit.com" /: "r" /: subredditName /: "new.json"
    options :: Option Https
    options =
      mconcat
        [ header "Accept" "application/json"
        , header "User-Agent" "haskell-reddit-test"
        , basicAuthHeaders
        , limit 100
        ]

newpostsSince :: Text -> Fullname -> Req (Either String Listing)
newpostsSince subredditName fullname =
  parseEither parseJSON . responseBody <$>
  req GET url_ NoReqBody jsonResponse options
  where
    url_ = https "reddit.com" /: "r" /: subredditName /: "new.json"
    options :: Option Https
    options =
      mconcat
        [ header "Accept" "application/json"
        , header "User-Agent" "haskell-reddit-test"
        , basicAuthHeaders
        , postsAfter fullname
        ]

baseReq :: Req (Either String Listing)
baseReq =
  parseEither parseJSON . responseBody <$>
  req
    GET
    (https "reddit.com" /: "r" /: "earthporn" /: "new.json")
    NoReqBody
    jsonResponse
    options
  where
    options :: Option Https
    options =
      mconcat
        [ header "Accept" "application/json"
        , header "User-Agent" "haskell-reddit-test"
        , basicAuthHeaders
        , limit 20
        ]

reqLink :: Link -> Req BS.ByteString
reqLink link = responseBody <$> req GET url_ NoReqBody bsResponse mempty
  where
    urlParts =
      trace' .
      Prelude.filter (\i -> i /= "http:" && i /= "https:") .
      Prelude.filter (not . T.null) . split (== '/') $
      url link
    urlHead = https . Prelude.head $ urlParts
    urlTail = Prelude.tail urlParts
    url_ = trace' $ Prelude.foldl (/:) urlHead urlTail

trace' x = trace (show x) x

downLink :: Link -> Req ()
downLink link = do
  r <- runReq defaultHttpConfig req
  let filename = unpack . Prelude.last . split (== '/') . url $ link
  liftIO $ BS.writeFile filename r
  where
    req = reqLink link

downTopTenImages :: Req ()
downTopTenImages = topImages >>= either (const $ pure ()) (V.mapM_ downLink)

limit :: Integer -> Option s
limit n = "limit" =: n

-- when pulling from new, the after Fullname you receive can be used with &before=Fullname to retrieve the posts after that post
postsAfter :: Fullname -> Option s
postsAfter name = "before" =: name

substream :: Text -> Stream (Of (Either String Listing)) IO ()
substream subredditName = do
  init <- liftIO . runReq defaultHttpConfig $ newposts subredditName
  let mostRecentPostFullname =
        maybe firstChildLeft Right . maybeHead . children . listingData =<< init
  S.yield init
  streamIOFix subrecurse' init
  where
    firstChildLeft = Left "No posts"
    subrecurse' :: Either String Listing -> IO (Either String Listing)
    subrecurse' x =
      runReq defaultHttpConfig $
      either (pure . Left) (subrecurse subredditName) x

subrecurse_ :: Text -> Fullname -> Stream (Of (Either String Listing)) IO ()
subrecurse_ subredditName mostRecentFullname = do
  newposts <-
    runReq defaultHttpConfig $ newpostsSince subredditName mostRecentFullname
  S.yield $ Left "aoeu"

subrecurse :: Text -> Listing -> Req (Either String Listing)
subrecurse subredditName listing =
  maybe
    (pure . Left $ "No new posts")
    (newpostsSince subredditName)
    (after . listingData $ listing)

maybeHead :: V.Vector a -> Maybe a
maybeHead v =
  if V.null v
    then Nothing
    else Just $ V.head v

newSubStream :: Text -> Stream (Of (Either String Listing)) IO ()
newSubStream subredditName = do
  init <- liftIO . runReq defaultHttpConfig $ newposts subredditName
  either noPosts hasPosts init
  where
    noPosts x = trace ("No posts" ++ show x) newSubStream subredditName
    hasPosts :: Listing -> Stream (Of (Either String Listing)) IO ()
    hasPosts listing = do
      liftIO . putStrLn . Prelude.take 50 . show $ listing
      S.yield $ pure listing
      either noPosts (okStartingOver subredditName) $ latestPostName listing

okStartingOver :: Text -> Fullname -> Stream (Of (Either String Listing)) IO ()
okStartingOver subredditName fullname = do
  newPosts <-
    liftIO . runReq defaultHttpConfig $ newpostsSince subredditName fullname
  S.yield newPosts
  let whenNoPosts = const $ okStartingOver subredditName fullname
  let whenNewPosts = okStartingOver subredditName
  let latestName = newPosts >>= latestPostName
  either whenNoPosts whenNewPosts latestName

latestPostName :: Listing -> Either String Fullname
latestPostName =
  maybe noNewPosts (Right . name) . maybeHead . children . listingData
  where
    noNewPosts = Left "No new posts" :: Either String a
-- so get init, yield it. if it has children, pass it to recurse. otherwise return what it was given
-- next fxn uses that fullname and tries to get items. 
