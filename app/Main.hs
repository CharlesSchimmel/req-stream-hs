{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Either
import Data.Text as T
import Streaming.Prelude as S

import Lib
import Types

main :: IO ()
main =
  S.mapM_ linePerTitle .
  S.take 10 . S.delay 30 . S.filter isRight . S.map postTitles $
  newSubStream "earthporn"
  where
    postTitles = fmap ((fmap $ T.unpack . title) . children . listingData)
    linePerTitle (Right titles) = Prelude.mapM_ putStrLn titles
    linePerTitle _ = pure ()
