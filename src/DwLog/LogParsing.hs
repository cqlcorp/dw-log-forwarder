{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             NoMonomorphismRestriction, OverloadedStrings #-}
module DwLog.LogParsing where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time
import           Text.Parsec
import           Text.Parsec.Text

import           DwLog

--------------------------------------------------------------------------------
-- customLogParser targets files of the format
--   customerror-bladeX-Y-appserver-YYYYMMDD.log
--------------------------------------------------------------------------------
customLogParser :: DwLogMeta -> Parser DwLog
customLogParser meta = do
  discard $ many endOfLine
  timestamp' <- timestampParser
  discard $ space
  level' <- tillSpace

  isSuppressed <- lookAheadOneOf ["RepeatedMessageSuppressingFilter-Thread"]

  logInnards' <- if isSuppressed
    then do
      return (emptyDwLog meta timestamp' CustomLog) { level = level' }
    else do
      servlet' <- tillPipe
      discard $ tillPipe

      let logBasics = (emptyDwLog meta timestamp' CustomLog) { level = level', servlet = servlet' }

      logInnards <-
        if servlet' == "JobThread"
          then do
            jobName' <- tillPipe
            pipeline' <- tillSpace
            return logBasics { pipeline = pipeline' , jobName = Just jobName' }
          else do
            site' <- siteParser (char '|')
            pipeline' <- tillPipe
            caller' <- tillPipe
            sessionID' <- tillSpace
            return logBasics { site = site' , pipeline = pipeline' , caller = caller' , sessionID = sessionID' }

      return logInnards

  logger' <- tillSpace
  discard $ spaces
  message' <- manyTill anyChar endOfLog
  return logInnards' { logger = logger' , message = T.strip $ T.pack message' }

--------------------------------------------------------------------------------
-- dwSystemLogParser targets files of the format
--   error-bladeX-Y-appserver-YYYYMMDD.log
--------------------------------------------------------------------------------
dwSystemLogParser :: DwLogMeta -> Parser DwLog
dwSystemLogParser meta = do
  discard $ many endOfLine
  timestamp' <- timestampParser
  discard $ space
  level' <- tillSpace

  let logBasics = (emptyDwLog meta timestamp' SystemLog) { timestamp = timestamp', level = level' }

  knownFormat <- lookAheadOneOf ["PipelineCallServlet", "JobThread"]

  if not knownFormat
    then do
      caller' <- tillSpace
      logger' <- tillSpace
      discard $ try (string " - - - - ") <|> return ""
      message' <- manyTill anyChar endOfLog
      return logBasics { caller = caller', logger = logger', message = T.strip $ T.pack message' }

    else do
      servlet' <- tillPipe
      discard $ tillPipe

      logInnards <-
        if servlet' == "JobThread"
          then do
            jobName' <- tillPipe
            pipeline' <- tillSpace
            return logBasics { jobName = Just $ jobName', pipeline = pipeline', caller = "Job Runner" }
          else do
            discard $ tillPipe -- this is site but relax, it's repeated later
            pipeline' <- tillPipe
            caller' <- tillPipe
            discard $ tillSpace -- this is sessionID but relax, it's repeated later
            return logBasics { pipeline = pipeline', caller = caller' }

      logger' <- tillSpace
      discard $ spaces

      site' <- siteParser (char ' ')
      sessionType' <- tillSpace
      sessionID' <- tillSpace
      requestID' <- tillSpace
      discard $ tillSpace -- some big number; not sure what it is

      discard $ spaces

      message' <- manyTill anyChar (lookToSystemInfo <|> endOfLog)

      systemInfo' <- option [] $ try $ keyValuePairs "System Information" ": "
      requestInfo' <- option [] $ try $ keyValuePairs "Request Information" ": "
      requestParams' <- option [] $ try $ keyValuePairs "Request Parameters" "="

      malformedRequestParams' <- T.pack <$> manyTillOneOf anyChar
        [ ignore strayHttpHeadersParser
        , ignore stackTraceParser
        , endOfLog
        ]

      strayHttpHeaders' <- optionMaybe $ try strayHttpHeadersParser
      stackTrace' <- optionMaybe $ try stackTraceParser

      return logInnards
        { servlet = servlet'
        , site = site'
        , sessionID = sessionID'
        , sessionType = Just $ sessionType'
        , requestID = Just $ requestID'
        , logger = logger'
        , message = T.strip $ T.pack message'
        , systemInfo = systemInfo'
        , requestInfo = requestInfo'
        , requestParams = requestParams'
        , malformedRequestParams = if T.null malformedRequestParams' then Nothing else Just malformedRequestParams'
        , strayHttpHeaders = strayHttpHeaders'
        , stackTrace = stackTrace'
        }

--------------------------------------------------------------------------------
-- Matches the key/value pairs in DW system error logs, e.g.
--
--   |Request Information\n
--   |-------------------\n
--   |URI: /whatever\n
--   |Remote Address: 1.2.3.4
--
-- The trailing eol is not consumed so that it plays nicely with our log
-- separator
--------------------------------------------------------------------------------
keyValuePairs :: Text -> Text -> Parser [(Text, Text)]
keyValuePairs header sep = do
  discard $ keyValuePairHeader header
  pairs <- option [] $ many $ keyValueLine sep
  return pairs

keyValuePairHeader :: Text -> Parser ()
keyValuePairHeader header = do
  discard $ many endOfLine
  discard $ string $ T.unpack header
  discard $ endOfLine
  discard $ count (T.length header) hyphen
  return ()

--------------------------------------------------------------------------------
-- e.g. "\nkey=val"
-- where the trailing eol is not absorbed because it would interfere with the
-- overall log separator, since there is no double eol before the next log
--------------------------------------------------------------------------------
keyValueLine :: Text -> Parser (Text, Text)
keyValueLine sep = try $ do
  let
    validKeyChar = alphaNum <|> oneOf "_-$&#;@."
    keyParser = do
      discard $ endOfLine
      keyHead <- validKeyChar
      keyTail <- manyTill (validKeyChar <|> char ' ') $ string (T.unpack sep)
      return $ T.pack $ keyHead : keyTail
  key <- keyParser
  val <- T.pack <$> manyTillOneOf anyChar
           [ ignore keyParser
           , ignore $ keyValuePairHeader "Request Information"
           , ignore $ keyValuePairHeader "Request Parameters"
           , ignore strayHttpHeadersParser
           , ignore stackTraceParser
           , endOfLog
           ]
  return (key, val)

ignore :: Parser a -> Parser ()
ignore m = m >> return ()

manyTillOneOf :: Parser a -> [Parser ()] -> Parser [a]
manyTillOneOf p =
  manyTill p . choice . fmap tryLookAhead

--------------------------------------------------------------------------------
-- Some logs have this weird http raw request looking thing.
--
--   |POST /on/demandware.store/Sites-mysite10_us-Site/default/Cart-AddProduct?format=ajax HTTP/1.1\n
--   |Host: www.mysite10.com\n
--   |Connection: keep-alive\n
--   |Content-Length: 174\n
--   |Accept: */*\n
--   |O
--------------------------------------------------------------------------------
strayHttpHeadersParser :: Parser Text
strayHttpHeadersParser = do
  discard $ endOfLine
  start <- string "POST "
  rest <- manyTill anyChar (endOfLog <|> tryLookAhead (stackTraceParser >> return ()))
  return $ T.concat [ T.pack start, T.pack rest ]

--------------------------------------------------------------------------------
-- Stack traces come in two forms, an ID with contents or a ref to an existing
-- stack trace.
--------------------------------------------------------------------------------
stackTraceParser :: Parser StackTrace
stackTraceParser =
  let
    contentsParser = do
      discard $ endOfLine
      discard $ string "Stack trace <"
      ref <- count 32 alphaNum
      discard $ char '>'
      discard $ endOfLine
      contents <- manyTill anyChar endOfLog
      return $ StackTrace (T.pack ref) (T.pack contents)

    refParser = do
      discard $ endOfLine
      discard $ string "Stack trace <ref:"
      ref <- count 32 alphaNum
      discard $ char '>'
      return $ StackTraceRef $ T.pack ref
  in try contentsParser <|> try refParser

--------------------------------------------------------------------------------
-- quotaLogParser targets files of the format
--   quota-bladeX-Y-appserver-YYYYMMDD.log
--------------------------------------------------------------------------------
quotaLogParser :: DwLogMeta -> Parser DwLog
quotaLogParser meta = do
  discard $ many endOfLine
  timestamp' <- timestampParser
  discard $ space
  discard $ char '['

  let logBasics = emptyDwLog meta timestamp' QuotaLog

  isMulticastListener <- (tryLookAhead (string "MulticastListener") >> return True) <|> return False

  logInnards <-
    if isMulticastListener
      then do
        caller' <- string "MulticastListener"
        discard $ many1 digit -- not sure what this is all about
        discard $ string "] "
        return logBasics { caller = T.pack caller' }
      else do
        servlet' <- tillPipe
        discard $ tillPipe -- not sure, some big number
        logInnards <- return logBasics { servlet = servlet' }
        if servlet' == "JobThread"
          then do
            jobName' <- tillPipe
            pipeline' <- manyTill anyChar $ string "] "
            return logInnards { pipeline = T.pack pipeline', jobName = Just jobName' }
          else do
            site' <- siteParser (char '|')
            pipeline' <- tillPipe
            caller' <- tillPipe
            sessionID' <- manyTill anyChar $ string "] "
            return logInnards { site = site', pipeline = pipeline', caller = caller', sessionID = T.pack sessionID' }

  (logger', quotaInfo') <- quotaInfoParser

  discard $ try (string ", ") <|> return ""

  message' <- manyTill anyChar endOfLog

  return logInnards
    { logger = logger'
    , quotaInfo = Just quotaInfo'
    , message = T.strip $ T.pack message'
    , level = if maxActual quotaInfo' >= limit quotaInfo' then "ERROR" else "WARN"
    }

quotaInfoParser :: Parser (Text, QuotaInfo)
quotaInfoParser = do
  discard $ string "Quota "
  logger' <- tillSpace
  discard $ char '('
  quotaType' <- many1 (alphaNum <|> char ' ')

  quotaWarn <- optionMaybe $ try $ do
    discard $ string ", "
    discard $ string "warn "
    intParser

  discard $ string ", "

  limit' <- string "limit " *> intParser

  discard $ string "): "
  discard $ choice $ string <$> [ "limit", "warn threshold" ]
  discard $ string " exceeded "
  exceededTimes' <- intParser
  discard $ string " time(s), max actual was "
  maxActual' <- intParser

  return $ (logger', QuotaInfo
    { warn = quotaWarn
    , limit = limit'
    , exceededTimes = exceededTimes'
    , maxActual = maxActual'
    , quotaType = T.pack quotaType'
    })

--------------------------------------------------------------------------------
-- apiLogParser targets files of the format
--   api-bladeX-Y-appserver-YYYYMMDD.log
--------------------------------------------------------------------------------
apiLogParser :: DwLogMeta -> Parser DwLog
apiLogParser meta = do
  discard $ many endOfLine
  timestamp' <- timestampParser
  discard $ space
  let logBasics = emptyDwLog meta timestamp' ApiLog
  let labeledPipelineParser = do
        discard $ try $ string "PIPELINE: "
        tillSpace
  let labeledSiteParser = do
        discard $ try $ string "SITE: "
        siteParser ((char ' ' >> return ()) <|> endOfLog)

  logInnards <- choice
    [ try $ do
        violation' <- manyTill anyChar (try $ string " violation: ")
        level' <- try (logLevelParser <* string ": ") <|> return ""
        message' <- manyTill anyChar (tryLookAhead $ choice [ labeledPipelineParser, labeledSiteParser, endOfLog >> return "" ])
        discard $ try $ many space
        pipeline' <- try labeledPipelineParser <|> return ""
        site' <- try labeledSiteParser <|> return ""
        return logBasics
          { message = T.strip $ T.pack message'
          , level = level'
          , pipeline = pipeline'
          , site = site'
          , apiInfo = Just $ emptyApiInfo { violation = Just $ T.pack violation' }
          }
    , try $ do
        method' <- tryLookAhead (string "API Method " *> tillSpace)
        if T.null method'
          then parserFail "Does not contain API Method"
          else do
            message' <- manyTill anyChar endOfLog
            return logBasics
              { message = T.strip $ T.pack message'
              , apiInfo = Just $ emptyApiInfo { method = Just method' }
              }
    , do
        message' <- manyTill anyChar endOfLog
        return logBasics { message = T.strip $ T.pack message' }
    ]

  return logInnards

--------------------------------------------------------------------------------
-- apiDeprecationLogParser targets files of the format
--   api-deprecation-bladeX-Y-appserver-YYYYMMDD.log
--------------------------------------------------------------------------------
apiDeprecationLogParser :: DwLogMeta -> Parser DwLog
apiDeprecationLogParser meta = do
  discard $ many endOfLine
  timestamp' <- timestampParser
  discard $ space
  let logBasics = emptyDwLog meta timestamp' ApiDeprecationLog
  let tillEndOfIn = T.pack <$> manyTill anyChar (try $ string " in ")

  logInnards <- choice
    [ do
        discard $ try $ string "SCRIPT_METHOD:"
        scriptMethod' <- tillEndOfIn
        return logBasics { deprecation = Just emptyDeprecationInfo { scriptMethod = Just scriptMethod' } }
    , do
        discard $ try $ string "DECISION_NODE:"
        decisionNode' <- tillEndOfIn
        return logBasics { deprecation = Just emptyDeprecationInfo { decisionNode = Just decisionNode' } }
    , do
        message' <- tillEndOfIn
        return logBasics { message = message' }
    ]

  logInnards2 <- choice
    [ do
        discard $ try $ string "request/site "
        site' <- siteParser (char '/')
        return logInnards { site = site' }
    , do
        discard $ try $ string "job/job "
        jobName' <- tillSlash
        return logInnards { jobName = Just jobName' }
    ]

  discard $ string "top pipeline "
  pipeline' <- tillSlash
  depWhere' <- T.pack <$> manyTill anyChar (tryLookAhead timesParser)
  times' <- timesParser

  return logInnards2
    { pipeline = pipeline'
    , deprecation = Just $
        (case deprecation logInnards2 of Nothing -> emptyDeprecationInfo; Just d -> d)
          { depWhere = depWhere'
          , times = times'
          }
    }
  where
    timesParser = try $ do
      discard $ string ", "
      times' <- intParser
      discard $ string " time(s)"
      discard $ endOfLog
      return times'

--------------------------------------------------------------------------------
-- e.g. [2016-06-08 00:41:23.325 GMT]
--------------------------------------------------------------------------------
timestampParser :: Parser LocalTime
timestampParser = do
  year <- char '[' *> count 4 digit <* hyphen
  month <- count 2 digit <* hyphen
  day <- count 2 digit <* space
  hour <- count 2 digit <* colon
  minute <- count 2 digit <* colon
  sec <- count 2 digit <* dot
  ms <- count 3 digit <* string " GMT]"
  return $
    LocalTime
      { localDay = fromGregorian (read year) (read month) (read day)
      , localTimeOfDay = TimeOfDay (read hour) (read minute) (read (sec ++ "." ++ ms))
      }

--------------------------------------------------------------------------------
-- Turns Sites-whatever-Site into whatever and Sites-Site into
-- dw-business-manager
--------------------------------------------------------------------------------
siteParser :: Parser a -> Parser Text
siteParser end =
  T.pack <$> choice
    [ try (string "Sites-" *> manyTill anyChar (string "-Site" >> end))
    , try (string "Sites-Site" >> end >> return "dw-business-manager")
    , manyTill anyChar end
    ]

--------------------------------------------------------------------------------
-- Common lookahead parsers
---------------------------------------------------------------------------------
lookToNextLog :: Parser ()
lookToNextLog =
  tryLookAhead $ do
    discard $ endOfLine
    discard $ timestampParser
    return ()

lookToSystemInfo :: Parser ()
lookToSystemInfo =
  tryLookAhead $ do
    discard $ endOfLine
    discard $ string "System Information"
    discard $ endOfLine
    return ()

endOfLog :: Parser ()
endOfLog =
  discard $ tryLookAhead lookToNextLog <|> tryLookAhead (many endOfLine >> eof)

--------------------------------------------------------------------------------
-- Shorthand parsers
--------------------------------------------------------------------------------
colon :: Parser Char
colon = char ':'

dot :: Parser Char
dot = char '.'

hyphen :: Parser Char
hyphen = char '-'

tillSpace :: Parser Text
tillSpace = T.pack <$> manyTill anyChar space

tillPipe :: Parser Text
tillPipe = T.pack <$> manyTill anyChar (char '|')

tillSlash :: Parser Text
tillSlash = T.pack <$> manyTill anyChar (char '/')

tillColon :: Parser Text
tillColon = T.pack <$> manyTill anyChar (char ':')

manyTill1 :: Parser Char -> Parser b -> Parser Text
manyTill1 p sep = do
  x <- p
  xs <- manyTill p sep
  return $ T.pack (x:xs)

tryLookAhead :: Parser a -> Parser a
tryLookAhead = try . lookAhead

intParser :: Parser Int
intParser = do
  hyphen' <- option "" $ string "-"
  digits <- many1 digit
  return $ (read (hyphen' ++ digits) :: Int)

logLevels :: [String]
logLevels = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]

logLevelParser :: Parser Text
logLevelParser = T.pack <$> choice (try . string <$> logLevels)

lookAheadOneOf :: [String] -> Parser Bool
lookAheadOneOf options =
  (choice ((tryLookAhead . string) <$> options) >> return True) <|> return False

-- so ghc doesn't force _ <- whatever
discard :: Monad m => m a -> m ()
discard m = m >> return ()
