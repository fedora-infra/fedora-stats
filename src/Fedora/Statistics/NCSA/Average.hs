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

import Data.Time.Calendar
import Fedora.Statistics.NCSA.Time
import Fedora.Statistics.NCSA.Types

data DateEntries =
  DateEntries { dateEntriesDate :: Day
              , dateEntriesEntries :: [LogEntry]
              } deriving (Eq, Ord, Show)

-- | Break up the requests into 10-minute chunks per day.
--groupRequests :: [LogEntry] ->

-- | Compute the average number of requests for a given hour of the day in 10
-- minute chunks.
--
-- We want to look at 10 minute chunks, so we go by fives, and add/subtract
-- 5 minutes each time, and filter the requests based on that.
