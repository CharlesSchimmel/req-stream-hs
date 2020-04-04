module Stream where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Streaming.Prelude as S

streamFix :: (a -> a) -> a -> Stream (Of a) IO ()
streamFix fn x = do
  let fnResult = fn x
  S.yield fnResult
  streamFix fn fnResult

streamIOFix :: (a -> IO a) -> a -> Stream (Of a) IO ()
streamIOFix fn x = do
  result <- liftIO . fn $ x
  S.yield result
  streamIOFix fn result
