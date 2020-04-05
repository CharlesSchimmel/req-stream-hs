# req-stream-hs

This is an exercise in using and understanding the Reddit API as well as playing
around with streams.

The Reddit library "PRAW" has a really nice way of creating infinite streams of
new posts for a given subreddit that can then be acted on. This little project
is an attempt to recreate that functionality in Haskell with the `Streaming`
library, using continuation-passing style

# Usage

The following example will fetch new posts from the chosen subreddit and print
their titles until 10 batches of posts has been received.

```
main :: IO ()
main = S.mapM_ linePerTitle .
    S.take 10 . S.delay 30 . S.filter isRight . S.map postTitles $
    newSubStream "mySubreddit"
```
