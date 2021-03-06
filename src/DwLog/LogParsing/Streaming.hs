{-# LANGUAGE OverloadedStrings #-}
module DwLog.LogParsing.Streaming
  ( parseStream
  , DetailedParseError(..)
  ) where

import           Data.List            (groupBy)
import           Data.Text            (Text)
import qualified Data.Text.Lazy       as LT
import           Text.Parsec
import           Text.Parsec.Text
import qualified Text.Regex.TDFA.Text as RGXT

import           DwLog
import           DwLog.LogParsing

--------------------------------------------------------------------------------
-- A wrapper for ParseError which retains the originally parsed log text
--------------------------------------------------------------------------------
data DetailedParseError = DetailedParseError ParseError Text

instance Show DetailedParseError where
  show (DetailedParseError err log') =
    unlines [ show err, "Original Log", "------------", show log' ]

--------------------------------------------------------------------------------
-- More efficiently parses a stream of lazy text. All the funny zipping and
-- tuples are there to provide better error messages
--------------------------------------------------------------------------------
parseStream :: Parser DwLog -> [RGXT.Regex] -> LT.Text -> [Either DetailedParseError DwLog]
parseStream parser omissionRegexes stream =
  LT.lines stream
    |> zip [1..] -- this adds line # for debug message
    |> groupBy notStartingWithTimestamp
    |> zip [1..] -- this adds log # for debug message
    |> fmap rejoinLogLinesIntoText
    |> filter (keepLog omissionRegexes)
    |> fmap (doParse parser)

notStartingWithTimestamp :: a -> (b, LT.Text) -> Bool
notStartingWithTimestamp _ (_, next) =
  not $ isTimestamp $ LT.toStrict next
  where
    isTimestamp line =
      case runP timestampParser () "" line of
        Right _ -> True
        Left _  -> False

rejoinLogLinesIntoText :: (a, [(b, LT.Text)]) -> (a, b, Text)
rejoinLogLinesIntoText (logNum, logLines) =
  let
    lineNum = fst $ head logLines
    text =
      map snd logLines
        |> LT.unlines
        |> LT.toStrict
  in
    (logNum, lineNum, text)

keepLog :: [RGXT.Regex] -> (Integer, Integer, Text) -> Bool
keepLog omissionRegexes (_, _, rawLog) =
  let matches rgx =
        case RGXT.execute rgx rawLog of
          Right (Just _) -> True
          _              -> False
  in not $ or $ fmap matches omissionRegexes

doParse :: Parser DwLog -> (Integer, Integer, Text) -> Either DetailedParseError DwLog
doParse parser (logNum, lineNum, log') =
  case runP parser () lineInfo log' of
    Left err -> Left $ DetailedParseError err log'
    Right ok -> Right ok
  where lineInfo = "log #" ++ show logNum ++ " (starting at line #" ++ show lineNum ++ " in stream)"

--------------------------------------------------------------------------------
-- It's just F#/Elm's pipe operator for forward piping. It makes more sense
-- to me than long chains of function composition
--------------------------------------------------------------------------------
infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)


