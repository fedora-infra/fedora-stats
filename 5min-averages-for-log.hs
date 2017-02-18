module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V
import Fedora.Statistics.NCSA

hour21 :: DiffTime -> DiffTime -> UTCTime
hour21 = genChunk (fromGregorian 2017 01 23) 21

main :: IO ()
main = do
  log' <- V.fromList . BL.lines <$> BL.readFile "/home/ricky/dev/fedora/haskell/staskell/sample-logs/1mil.log"
  let entries = parseFileLines parseLogEntry log'
  print $ fmap V.length (groupByFiveMinuteChunks entries hour21)
