{-# LANGUAGE OverloadedStrings #-}
module DwLog.LogParsing.IO where

import           Codec.Compression.GZip
import qualified Data.ByteString.Lazy       as BSL
import           Data.List                  (isSuffixOf)
import qualified Data.Text.Lazy.Encoding    as LTE
import qualified Data.Text.Lazy.IO          as LTIO
import qualified Text.Regex.TDFA.Text       as RGXT

import           DwLog
import           DwLog.LogParsing.Streaming

parseFromFile :: DwLogParser -> [RGXT.Regex] -> String -> IO [Either DetailedParseError DwLog]
parseFromFile p omissionRegexes path = do
  input <- if isSuffixOf ".gz" path
             then do
               stream <- BSL.readFile path
               return $ LTE.decodeUtf8 $ decompress stream
             else LTIO.readFile path
  return $ parseStream p omissionRegexes input
