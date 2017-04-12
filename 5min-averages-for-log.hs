{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Thyme.Clock
import Data.Thyme.Time
import qualified Data.Vector as V
import Fedora.Statistics.NCSA

hours :: [DiffTime -> DiffTime -> UTCTime]
hours = fmap (genChunk (fromGregorian 2017 03 23) . fromIntegral) [0..23::Int]

average :: (Fractional a, Integral a1, Foldable t) => t a1 -> a
average x = fromIntegral (sum x) / fromIntegral (length x)

main :: IO ()
main = do
  log' <- V.fromList . BL.lines <$> BL.readFile "/home/ricky/devel/fedora/staskell/hotspot-ten-percent.log"
  let entries = parseFileLines parseLogEntry log'
      groups = hours >>= groupByFiveMinuteChunks entries
      groupsLen = filter (/= 0) (fmap length groups)
      groupsAvg = average groupsLen

      --lengths = fmap V.length grouped
  putStrLn . show $ groupsAvg

