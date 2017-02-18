-----------------------------------------------------------------------------
-- |
-- Module      :  Fedora.Statistics.NCSA.Average
-- Copyright   :  (C) 2017 Red Hat, Inc.
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Ricky ELrod <relrod@redhat.com>
-- Stability   :  provisional
-- Portability :  non-portable
----------------------------------------------------------------------------
module Fedora.Statistics.NCSA.Average where

import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Vector as V
import Fedora.Statistics.NCSA.Time
import Fedora.Statistics.NCSA.Types

data DateEntries =
  DateEntries { dateEntriesDate :: Day
              , dateEntriesEntries :: [LogEntry]
              } deriving (Eq, Ord, Show)

-- |
-- Given a list of log entries and an hour, we want to break up that hour into
-- five minute chunks and then generate a list of log entries in each of those
-- chunks.
groupByFiveMinuteChunks
  :: V.Vector LogEntry
  -> (DiffTime -> DiffTime -> UTCTime)
  -> [V.Vector LogEntry]
groupByFiveMinuteChunks entries f =
  let chunks = fiveMinuteChunks f
  in fmap (\c -> filterTimes entries time c 150) chunks
