{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Config
import qualified Control.Exception          as Ex
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.List
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time
import           LogShipperDb
import           Network.Connection         (TLSSettings (..))
import           Network.HTTP.Client.TLS    (mkManagerSettings)
import           Network.Wreq
import           SmtpLogHandler
import           System.Directory
import           System.Exit                (exitFailure)
import           System.FilePath.Posix      (takeFileName, (</>))
import           System.Log.Formatter       (simpleLogFormatter)
import           System.Log.Handler         (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Parsec
import           Text.Parsec.Text
import           Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text       as RGXT

import           DwLog
import           DwLog.LogParsing
import           DwLog.LogParsing.IO
import           DwLog.LogParsing.Streaming
import           DwLog.LogParsing.WebDav

main :: IO ()
main = do
  config <- getConfig
  configureLogging config
  Ex.handle handler $ do
    instances <- getInstances config
    forM_ instances $ doInstance config
  where
    handler ex = do
      criticalM rootLoggerName $ "Unhandled exception: " ++ show (ex :: Ex.SomeException)
      exitFailure

getConfig :: IO Config
getConfig = do
  let configFile = "app.config"
  configFileExists <- doesFileExist configFile
  if not configFileExists
    then error $ "Could not find configuration: " ++ configFile
    else do loadConfig configFile

configureLogging :: Config -> IO ()
configureLogging config = do
  utcToday <- getUtcToday
  createDirectoryIfMissing True $ appLogFolder config
  let filePath' = (appLogFolder config) </> ("dw-log-forwarder-" ++ (T.unpack $ toYYYYMMDD utcToday) ++ ".log")
  let useFormatter lh = return $ setFormatter lh (simpleLogFormatter "[$utcTime] $prio $loggername - $msg")
  logFile <- fileHandler filePath' INFO >>= useFormatter
  emailer <- smtpLogHandler (diagnostics config) ERROR >>= useFormatter

  updateGlobalLogger rootLoggerName (setLevel INFO . addHandler emailer . addHandler logFile)

doInstance :: Config -> DbInstance -> IO ()
doInstance config inst = do
  let domain = i_domain inst; username' = i_username inst; password' = i_password inst; instanceID = i_instanceID inst

  webdavAuthResult <- getWebDavAuth username' password'
  case webdavAuthResult of
    Left err -> errorM rootLoggerName $ T.unpack err
    Right webdavAuth' -> do
      let env = WebDavEnvironment domain webdavAuth'

      omissionRegexes <- getLogOmissionRegexes config instanceID

      tryFetchQueuedArchives config inst env
      tryProcessQueuedDayFiles config inst omissionRegexes

      infoM rootLoggerName $ "Querying logs from instance: " ++ T.unpack domain
      rootLogFiles <- getFolderContents env Root
      utcToday <- getUtcToday

      -- the deprecation folder goes back months, so let's just query yesterday and today
      depLogFiles <- filterFolderContents env Deprecation (keepPast24Hrs utcToday)

      case concat <$> sequence [rootLogFiles, depLogFiles] of
        Left err -> errorM rootLoggerName $ "Error fetching log files from " ++ T.unpack domain ++ "\n" ++ T.unpack err
        Right logFiles -> do
          knownLogFiles <- getLogFilesForInstance config instanceID
          let findKnownLogFile lf = find ((==) lf . lf_filePath) knownLogFiles
          let dropUnmodified = filter modified
                where modified (srvlf, dblf) =
                        case dblf of
                          Nothing -> True
                          Just lf -> utcServerLastModified srvlf > lf_utcServerLastModified lf
          let zippedLogFiles = dropUnmodified $ map (\lf -> (lf, findKnownLogFile (filePath lf))) logFiles
          forM_ zippedLogFiles $ doLogFile config inst env omissionRegexes

doLogFile :: Config -> DbInstance -> WebDavEnvironment -> [RGXT.Regex] -> (ServerLogFile, Maybe DbLogFile) -> IO ()
doLogFile config inst env omissionRegexes (serverLogFile, dbLogFile) = do
  let filePath' = filePath serverLogFile
  let utcServerLastModified' = utcServerLastModified serverLogFile
  let fileName = takeFileName $ T.unpack filePath'
  let meta = DwLogMeta (i_clientName inst) (T.pack fileName) (i_environment inst)

  case getParserForFile meta of
    Nothing -> return ()
    Just (parser, parserName) -> do
      infoM rootLoggerName $ "Log File: " ++ T.unpack filePath'

      let startByte = fromMaybe 0 $ lf_bytesRead <$> dbLogFile
      infoM rootLoggerName $ "Using parser " ++ T.unpack parserName
      case dbLogFile of
        Nothing -> infoM rootLoggerName "This is the first time we've seen this file"
        Just dblf -> infoM rootLoggerName $ "We've read " ++ show (lf_bytesRead dblf) ++ " bytes from this file so far"
      tail' <- tailLogFile env startByte filePath'
      case tail' of
        Left err -> errorM rootLoggerName $ T.unpack err
        Right (bytesRead, stream) -> do
          let jsonStream = parseStream parser omissionRegexes stream
          streamJsonToOutput config inst (takeFileName $ T.unpack filePath') jsonStream

          infoM rootLoggerName $ "Read " ++ show bytesRead ++ " bytes"
          incrementBytesRead config (i_instanceID inst) filePath' utcServerLastModified' bytesRead

streamJsonToOutput :: Config -> DbInstance -> String -> [Either DetailedParseError DwLog] -> IO ()
streamJsonToOutput config inst fileName jsonStream = do
  let jsonLines = BSL8.unlines (logToJson inst fileName <$> jsonStream)
  case outputFolder $ output config of
    Nothing -> return ()
    Just outputFolder' -> do
      let instanceDir = outputFolder' </> T.unpack (i_domain inst)
      createDirectoryIfMissing True instanceDir
      let outputFile = instanceDir </> (fileName ++ ".jsonlogs")
      BSL8.appendFile outputFile jsonLines
      infoM rootLoggerName $ "Appended JSON to output file: " ++ outputFile

  case logstashUrl $ output config of
    Nothing -> return ()
    Just logstashUrl' -> do
      infoM rootLoggerName $ "Forwarding to LogStash: " ++ logstashUrl'
      let opts = defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
      r <- postWith opts logstashUrl' jsonLines
      infoM rootLoggerName $ "LogStash response: " ++ (show $ r ^. responseStatus)

tryFetchQueuedArchives :: Config -> DbInstance -> WebDavEnvironment -> IO ()
tryFetchQueuedArchives config inst env = do
  loop
  where
    loop = do
      mayday <- getNextDayToFetch config (i_instanceID inst)
      case mayday of
        Nothing -> return ()
        Just day -> do
          deprecation' <- filterFolderContents env Deprecation (keepDays [day])
          archives <- filterFolderContents env LogArchive (keepDays [day])
          rootLogs <- filterFolderContents env Root (keepDays [day])

          let instanceDir = archiveQueueDir config </> T.unpack (i_domain inst)
          createDirectoryIfMissing True instanceDir

          case concat <$> sequence [deprecation', archives, rootLogs] of
            Left err -> errorM rootLoggerName $ "Error fetching archive log files:\n" ++ T.unpack err
            Right files' -> do
              forM_ files' (downloadLogFileToDisk config inst env instanceDir)
              removeDayFromFetchQueue config (i_instanceID inst) day
              loop

downloadLogFileToDisk :: Config -> DbInstance -> WebDavEnvironment -> String -> ServerLogFile -> IO ()
downloadLogFileToDisk _ _ env instanceDir file = do
  let serverPath = filePath file
  infoM rootLoggerName $ "Downloading full day file from " ++ show serverPath
  downloadResult <- downloadFile env serverPath
  case downloadResult of
    Left err -> errorM rootLoggerName $ "Error downloading file: " ++ T.unpack err
    Right contents -> do
      let localPath = instanceDir </> takeFileName (T.unpack serverPath)
      infoM rootLoggerName $ "Writing file to: " ++ localPath
      BSL.writeFile localPath contents

tryProcessQueuedDayFiles :: Config -> DbInstance -> [RGXT.Regex] -> IO ()
tryProcessQueuedDayFiles config inst omissionRegexes = do
  let instanceDir = archiveQueueDir config </> T.unpack (i_domain inst)
  let processedDir = instanceDir </> "processed"
  start <- getCurrentTime
  let runTil = addUTCTime (realToFrac $ maxDayQueueProcessSeconds config) start
  let
    loop = do
      now <- getCurrentTime
      if now > runTil
      then infoM rootLoggerName "We are stopping the processing of queued files because we hit the maxDayQueueProcessSeconds limit. Subsequent runs of this application will pick up remaining queued day logs"
      else do
        createDirectoryIfMissing True instanceDir
        files <- getDirectoryContents instanceDir
        case filter isLogFile files of
          [] -> return ()
          (file:_) -> do
            createDirectoryIfMissing True processedDir
            let filePath' = instanceDir </> file
            let meta = DwLogMeta (i_clientName inst) (T.pack file) (i_environment inst)

            case getParserForFile meta of
              Nothing -> return ()
              Just (parser, parserName) -> do
                infoM rootLoggerName $ "Parsing queued file: " ++ filePath'
                infoM rootLoggerName $ "Using parser: " ++ T.unpack parserName

                jsonStream <- parseFromFile parser omissionRegexes filePath'
                streamJsonToOutput config inst (takeFileName filePath') jsonStream

            let processedPath = processedDir </> file
            infoM rootLoggerName $ concat [ "Moving file ", filePath', " to ", processedPath ]
            renameFile filePath' processedPath
            loop
  loop

logToJson :: DbInstance -> String -> Either DetailedParseError DwLog -> BSL8.ByteString
logToJson inst fileName log' =
  case log' of
    Left err -> encode $ DetailedParseErrorJSON err inst fileName
    Right l  -> encode l

--------------------------------------------------------------------------------
-- JSON encoding of parse failures
--------------------------------------------------------------------------------
data DetailedParseErrorJSON = DetailedParseErrorJSON DetailedParseError DbInstance String

instance ToJSON DetailedParseErrorJSON where
  toJSON (DetailedParseErrorJSON (DetailedParseError parseError rawLog) inst fileName) =
    let tryParseTimestamp =
          case runP timestampParser () "" rawLog of
            Right ts -> Just ts
            Left _   -> Nothing
    in objectOmitNullAndEmpties
        [ "@timestamp" .= tryParseTimestamp
        , "type" .= T.pack "parse-error"
        , "client" .= T.toLower (i_clientName inst)
        , "logFile" .= fileName

        -- logstash won't let you override 'host', so call it 'server'
        , "server" .= parseHost (T.pack fileName)

        , "parseError" .= show parseError
        , "rawLog" .= rawLog
        , "environment" .= i_environment inst
        ]

getParserForFile :: DwLogMeta -> Maybe (Parser DwLog, Text)
getParserForFile logMeta
  | "api-deprecation-" `T.isPrefixOf` fn = Just (apiDeprecationLogParser logMeta, "apiDeprecationLogParser")
  | "api-" `T.isPrefixOf` fn = Just (apiLogParser logMeta, "apiLogParser")
  | "custom" `T.isPrefixOf` fn = Just (customLogParser logMeta, "customLogParser")
  | isPrefixOfWithLogLevels (flip T.append "-") = Just (dwSystemLogParser logMeta, "dwSystemLogParser")
  | "quota-" `T.isPrefixOf` fn = Just (quotaLogParser logMeta, "quotaLogParser")
  | otherwise = Nothing
  where
    fn = filename logMeta
    isPrefixOfWithLogLevels f =
      or $ fmap (`T.isPrefixOf` fn) (f <$> ["warn", "error", "fatal"])

keepPast24Hrs :: Day -> ServerLogFile -> Bool
keepPast24Hrs utcToday =
  keepDays [ utcToday, addDays (-1) utcToday ]

keepDays :: [Day] -> ServerLogFile -> Bool
keepDays days logFile =
  let regexes = (\x -> concat ["-", T.unpack (toYYYYMMDD x), "\\.log(\\.gz)?$"]) <$> days
  in or $ (\rgx -> (T.unpack $ filePath logFile) =~ rgx) <$> regexes

isLogFile :: String -> Bool
isLogFile path =
  let pat :: String; pat = "\\.log(\\.gz)?$"
  in path =~ pat

getUtcToday :: IO Day
getUtcToday = do
  now <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay now
  return $ fromGregorian y m d

toYYYYMMDD :: Day -> Text
toYYYYMMDD = T.pack . formatTime defaultTimeLocale "%Y%m%d"

endsWith :: Text -> Text -> Bool
endsWith needle haystack =
  case T.stripSuffix needle haystack of
    Nothing -> False
    Just _  -> True

