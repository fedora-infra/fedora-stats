{-# LANGUAGE CPP #-}
-- | Benchmark ALL THE THINGS
module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Criterion.Main
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Fedora.Statistics.NCSA

sampleLines :: BL.ByteString
sampleLines = BL.pack
  "94.19.26.210 - - [23/Jan/2017:21:23:26 +0000] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \2003:5b:4c2e:6990:5536:d5ff:fab5:4192 - - [23/Jan/2017:21:23:26 +0000] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \2a02:810a:8c00:2ea3:5cae:cb97:a612:5102 - - [23/Jan/2017:19:21:26 +0000] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \80.210.229.17 - - [23/Jan/2017:20:23:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \85.131.156.57 - - [23/Jan/2017:21:24:23 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \81.251.113.92 - - [23/Jan/2017:21:29:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \194.8.186.164 - - [23/Jan/2017:21:32:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \195.202.137.140 - - [23/Jan/2017:21:32:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \91.244.167.216 - - [23/Jan/2017:21:23:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \126.151.134.58 - - [23/Jan/2017:21:23:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \61.152.56.146 - - [23/Jan/2017:21:23:26 -0500] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \194.90.160.222 - - [23/Jan/2017:21:23:26 +0000] \"GET /static/hotspot.txt HTTP/1.0\" 200 2 \"-\" \"-\"\n\
  \73.155.15.131 - - [23/Jan/2017:21:23:26 +0000] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n\
  \145.56.120.128 - - [23/Jan/2017:21:23:26 +0000] \"GET /static/hotspot.txt HTTP/1.1\" 200 2 \"-\" \"-\"\n"

entries :: V.Vector LogEntry
entries = parseFileLines parseLogEntry . V.fromList . BL.lines $ sampleLines

twentyOne :: (DiffTime -> DiffTime -> UTCTime)
twentyOne = genChunk (fromGregorian 2017 01 23) 21

-- | 1h 23m
chunk :: UTCTime
chunk = UTCTime (fromGregorian 2017 01 23) 76980

main :: IO ()
main =
  defaultMain [
    bgroup "NCSA/Average"
    [
      bench "groupByFiveMinuteChunks" $ whnf (groupByFiveMinuteChunks entries) twentyOne
    ]
  , bgroup "NCSA/Time"
    [
      bench "genChunk" $ whnf (genChunk (fromGregorian 2017 01 23)) 21
    , bench "fiveMinuteChunks" $ whnf fiveMinuteChunks twentyOne
    , bench "filterTimes" $ whnf (filterTimes entries time chunk) 150
    ]
  ]
