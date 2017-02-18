module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Calendar
import Data.Time.Clock
import Fedora.Statistics.NCSA

hour21 :: DiffTime -> DiffTime -> UTCTime
hour21 = genChunk (fromGregorian 2017 01 23) 21

main :: IO ()
main = do
  log' <- BL.lines <$> BL.readFile "/home/ricky/dev/fedora/haskell/staskell/sample-logs/1mil.log"
  let entries = parseFileLines parseLogEntry log'
  print $ fmap length (groupByFiveMinuteChunks entries hour21)
