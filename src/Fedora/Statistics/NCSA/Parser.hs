{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Fedora.Statistics.NCSA.Parser
-- Copyright   :  (C) 2017 Red Hat, Inc.
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Ricky Elrod <relrod@redhat.com>
-- Stability   :  provisional
-- Portability :  non-portable
----------------------------------------------------------------------------
module Fedora.Statistics.NCSA.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Either (isRight)
import Data.Time
import qualified Data.Vector as V
import Fedora.Statistics.NCSA.Types

#if MIN_VERSION_time(1,5,0)
#else
import System.Locale

parseTimeOrError  :: ParseTime t => Bool -> TimeLocale -> String -> String -> t
parseTimeOrError _ = readTime
#endif

-- 97.70.28.123 - - [28/Jan/2017:04:02:09 +0000] "GET /static/hotspot.txt HTTP/1.1" 200 2 "-" "-"

-- | A helper which lets us parse strings with escaped quotes in them.
takeEscapedWhile :: (Char -> Bool) -> (Char -> Bool) -> Parser B.ByteString
takeEscapedWhile isEscapable while = do
  x <- normal
  xs <- many escaped
  return $ B.concat (x:xs)
  where
    normal = A.takeWhile (\c -> c /= '\\' && while c)
    escaped = do
      x <- (char '\\' *> satisfy isEscapable) <|> char '\\'
      xs <- normal
      return $ B.cons x xs

-- | Rather than parsing the actual IP out (and having to deal with the
-- intricacies that come along with parsing IPv6, we just parse up to the first
-- space for now.
parseIP :: Parser B.ByteString
parseIP = takeWhile1 (not . isSpace)
{-# INLINE parseIP #-}

-- | Similar to the above, we just parse until the next space.
parseIdentity :: Parser B.ByteString
parseIdentity = takeWhile1 (not . isSpace)
{-# INLINE parseIdentity #-}

-- | And again.
parseUser :: Parser B.ByteString
parseUser = takeWhile1 (not . isSpace)
{-# INLINE parseUser #-}

parseReqTime :: Parser UTCTime
parseReqTime = do
  char '['
  timestamp <- takeWhile1 (\x -> x /= ']')
  char ']'
  let fmtString = "%d/%b/%Y:%H:%M:%S %z"
  return (parseTimeOrError True defaultTimeLocale fmtString (B.unpack timestamp))

parseRequest :: Parser Request
parseRequest = do
  char '"'
  method' <- takeWhile1 (not . isSpace)
  many1 space
  path' <- takeWhile1 (not . isSpace)
  many1 space
  version' <- takeWhile1 (\x -> x /= '"')
  char '"'
  return (Request method' path' version')

parseStatus :: Parser Int
parseStatus = decimal
{-# INLINE parseStatus #-}

parseBytes :: Parser (Maybe Integer)
parseBytes =
  choice $ map try [
    do char '-'
       return Nothing,
    do fmap Just decimal
  ]

parseReferrer :: Parser B.ByteString
parseReferrer = do
  char '"'
  takeEscapedWhile (== '"') (/= '"') <* char '"'

parseUserAgent :: Parser B.ByteString
parseUserAgent = do
  char '"'
  takeEscapedWhile (== '"') (/= '"') <* char '"'

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  ip' <- parseIP
  many' space
  identity' <- parseIdentity
  many' space
  user' <- parseUser
  many' space
  time' <- parseReqTime
  many' space
  request' <- parseRequest
  many' space
  status' <- parseStatus
  many' space
  bytes' <- parseBytes
  many' space
  referrer' <- parseReferrer
  many' space
  useragent' <- parseUserAgent
  return $
    LogEntry
    ip'
    identity'
    user'
    time'
    request'
    status'
    bytes'
    referrer'
    useragent'

parseFileLines :: (Parser LogEntry) -> V.Vector BL.ByteString -> V.Vector LogEntry
parseFileLines parser rawLogLines =
  vRights $ fmap (parseFileLine parser) rawLogLines
  where
    vRights = fmap (\(Right x) -> x) . V.filter isRight

parseFileLine :: (Parser LogEntry) -> BL.ByteString -> Either String LogEntry
parseFileLine p logFileLine = parseOnly p ln
    where
        ln = (B.concat . BL.toChunks) logFileLine

