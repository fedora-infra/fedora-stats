module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Fedora.Statistics.NCSA

main :: IO ()
main = do
  log' <- BL.lines <$> BL.readFile "/mnt/fedora_stats/combined-http/2017/02/01/fedoraproject.org-access.log"
  print $ length (parseFileLines parseLogEntry log')
