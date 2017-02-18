{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Fedora.Statistics.NCSA.Types
-- Copyright   :  (C) 2017 Red Hat, Inc.
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Ricky ELrod <relrod@redhat.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Types for the various NCSA modules.
----------------------------------------------------------------------------
module Fedora.Statistics.NCSA.Types where

import qualified Data.ByteString.Char8 as B
import Data.Time

-- | This is a type which describes a log entry that has been parsed.
data LogEntry =
  LogEntry { host      :: B.ByteString
           , identd    :: B.ByteString
           , user      :: B.ByteString
           , time      :: UTCTime
           , request   :: Request
           , status    :: Int
           , bytes     :: Maybe Integer
           , referrer  :: B.ByteString
           , useragent :: B.ByteString
           } deriving (Eq, Ord, Show)

data Request =
  Request { method  :: B.ByteString
          , path    :: B.ByteString
          , version :: B.ByteString
          } deriving (Eq, Ord, Show)
