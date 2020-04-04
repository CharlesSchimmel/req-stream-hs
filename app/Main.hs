module Main where

import Data.Either
import Streaming.Prelude as S

import Lib
import Types

main :: IO ()
main =
  S.print . S.take 10 . S.delay 30 . S.filter isRight . S.map postTitles $
  newSubStream "earthporn"
  where
    postTitles = fmap ((fmap title) . children . listingData)
