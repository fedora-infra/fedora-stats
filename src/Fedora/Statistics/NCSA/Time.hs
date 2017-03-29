-----------------------------------------------------------------------------
-- |
-- Module      :  Fedora.Statistics.NCSA.Time
-- Copyright   :  (C) 2017 Red Hat, Inc.
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Ricky Elrod <relrod@redhat.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module contains functions for dealing with time in various ways, such
-- as splitting hours into chunks of minutes and filtering out times from
-- ranges.
--
-- This module is (intentionally) unaware of 'LogEntry'. All functions here are
-- made to be modular, so that eventually this module can be pulled out into a
-- more common library, since we will likely find these functions useful
-- elsewhere. Who knows, others might as well.
----------------------------------------------------------------------------
module Fedora.Statistics.NCSA.Time where

import Control.Applicative
import Control.Monad
import Data.AffineSpace
import Data.AdditiveGroup
import Data.Thyme.Calendar (Day)
import Data.Thyme.Clock
import Data.Thyme.Time

-- | Given a ['UTCTime'], a 'UTCTime', and a 'DiffTime', filter out things in
-- the original list that are within the 'DiffTime' range of the given
-- 'UTCTime'.
filterTimes
  :: MonadPlus m
  => m a
  -> (a -> UTCTime)
  -> UTCTime
  -> NominalDiffTime
  -> m a
filterTimes ts f u d =
  mfilter (\t -> abs ((f t) .-. u) - realToFrac d < 0) ts

-- | Generate a center point \"chunk\" for filtering.
--
-- You are unlikely to use this function by itself. The more useful
-- 'fiveMinuteChunks' below (or your own custom function similar to it) is
-- likely to be used with this function in some way.
genChunk
  :: Day -- ^ The day we are filtering for
  -> DiffTime -- ^ The hour of the day we are breaking up (0-23)
  -> DiffTime -- ^ The lower bound of the chunk
  -> DiffTime -- ^ The upper bound of the chunk
  -> UTCTime -- ^ A 'UTCTime' to use as the chunk
genChunk d hr lb ub = mkUTCTime d ((3600 * hr) + (((ub + lb) / 2) * 60))

-- | Generate a list of __center points__ used for filtering times.
--
-- An example use of this function might be something like the following:
--
-- @
--   twelve = 'genChunk' ('fromGregorian' 2017 02 01) 12
--   chunks = 'fiveMinuteChunks' twelve
-- @
--
-- This gets you a list of all the __center points__ of all of the five minute
-- chunks within the 12:00 hour on 2017-02-01. This can be used with
-- 'filterTimes' above by giving it an offset of @150@ seconds (that is,
-- 'filterTimes' will add and subtract 150 seconds from the center point to form
-- its filtering range).
fiveMinuteChunks :: (DiffTime -> DiffTime -> UTCTime) -> [UTCTime]
fiveMinuteChunks f =
  uncurry f <$> fmap (\x -> (x, x + 5)) [0, 5 .. 55]
