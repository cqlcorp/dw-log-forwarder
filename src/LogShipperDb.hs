{-# LANGUAGE OverloadedStrings #-}
module LogShipperDb where

import           Data.Text            as T
import           Data.Time
import           Database.HDBC
import           Database.HDBC.ODBC
import           Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as RGXT

import           Config

data DbInstance = DbInstance
  { i_clientName  :: Text
  , i_instanceID  :: Int
  , i_domain      :: Text
  , i_environment :: Text
  , i_username    :: Text
  , i_password    :: Text
  } deriving (Show)

data DbLogFile = DbLogFile
  { lf_instanceID            :: Int
  , lf_filePath              :: Text
  , lf_bytesRead             :: Integer
  , lf_utcServerLastModified :: LocalTime
  , lf_utcLastPolled         :: LocalTime
  } deriving (Show)

getInstances :: Config -> IO [DbInstance]
getInstances config = withConn config $ \conn -> do
  let sql =
        "SELECT\n\
        \  c.ClientName,\n\
        \  i.InstanceID,\n\
        \  i.Domain,\n\
        \  i.Environment,\n\
        \  i.Username,\n\
        \  CAST(DECRYPTBYPASSPHRASE(?, i.EncPassword) AS NVARCHAR(255)) Password\n\
        \FROM dw.Instance i\n\
        \  JOIN dbo.Client c ON i.ClientID = c.ClientID\n\
        \WHERE i.PollingEnabled = 1"
  results <- quickQuery' conn sql [ toSql $ encPasswordPassphrase config ]
  return $ toInstance <$> results
  where
    toInstance row =
      case row of
        (c1:c2:c3:c4:c5:c6:[]) ->
          DbInstance (fromSql c1) (fromSql c2) (fromSql c3) (fromSql c4) (fromSql c5) (fromSql c6)
        _ -> error $ "Invalid result from getInstancesForClient: " ++ show row

getLogFilesForInstance :: Config -> Int -> IO [DbLogFile]
getLogFilesForInstance config instanceID = withConn config $ \conn -> do
  let sql =
        "SELECT\n\
        \  lf.InstanceID,\n\
        \  lf.FilePath,\n\
        \  lf.BytesRead,\n\
        \  lf.UtcServerLastModified,\n\
        \  lf.UtcLastPolled\n\
        \FROM dw.LogFile lf\n\
        \WHERE lf.InstanceID = ?\n"
  results <- quickQuery' conn sql [ toSql instanceID ]
  return $ toLogFile <$> results
  where
    toLogFile row =
      case row of
        (c1:c2:c3:c4:c5:[]) ->
          DbLogFile (fromSql c1) (fromSql c2) (fromSql c3) (fromSql c4) (fromSql c5)
        _ -> error $ "Invalid result from getLogFilesForInstance: " ++ show row

incrementBytesRead :: Config -> Int -> Text -> LocalTime -> Integer -> IO ()
incrementBytesRead config instanceID filePath utcServerLastModified bytesRead =
  withConn config $ \conn -> do
    let sql =
          "UPDATE lf\n\
          \SET\n\
          \  BytesRead = lf.BytesRead + ?,\n\
          \  UtcServerLastModified = ?,\n\
          \  UtcLastPolled = GETUTCDATE()\n\
          \FROM dw.LogFile lf\n\
          \WHERE lf.InstanceID = ?\n\
          \  AND lf.FilePath = ?\n\
          \\n\
          \IF @@ROWCOUNT = 0\n\
          \BEGIN\n\
          \  INSERT dw.LogFile (InstanceID, FilePath, BytesRead, UtcServerLastModified, UtcLastPolled)\n\
          \  VALUES (?, ?, ?, ?, GETUTCDATE())\n\
          \END\n"
    _ <- run conn sql
      [ toSql bytesRead
      , toSql utcServerLastModified
      , toSql instanceID
      , toSql filePath
      , toSql instanceID
      , toSql filePath
      , toSql bytesRead
      , toSql utcServerLastModified
      ]
    return ()

getNextDayToFetch :: Config -> Int -> IO (Maybe Day)
getNextDayToFetch config instanceID = withConn config $ \conn -> do
  let sql =
        "SELECT TOP 1 Year, Month, Day\n\
        \FROM dw.DayFetchQueue q\n\
        \WHERE q.InstanceID = ?\n\
        \ORDER BY Year, Month, Day"
  results <- quickQuery' conn sql [ toSql instanceID ]
  return $ case results of
    []      -> Nothing
    (row:_) -> Just $ toLogFile row
  where
    toLogFile row =
      case row of
        (year:month:day:[]) ->
          fromGregorian (fromSql year) (fromSql month) (fromSql day)
        _ -> error $ "Invalid result from getNextDayToFetch: " ++ show row

removeDayFromFetchQueue :: Config -> Int -> Day -> IO ()
removeDayFromFetchQueue config instanceID day = withConn config $ \conn -> do
    let sql =
          "DELETE q\n\
          \FROM dw.DayFetchQueue q\n\
          \WHERE q.InstanceID = ?\n\
          \  AND q.Year = ?\n\
          \  AND q.Month = ?\n\
          \  AND q.Day = ?"
    let (year, month, day') = toGregorian day
    _ <- run conn sql
      [ toSql instanceID
      , toSql year
      , toSql month
      , toSql day'
      ]
    return ()

getLogOmissionRegexes :: Config -> Int -> IO [RGXT.Regex]
getLogOmissionRegexes config instanceID = withConn config $ \conn -> do
  let sql =
        "SELECT r.Regex\n\
        \FROM dw.LogOmissionRegex r\n\
        \WHERE r.InstanceID = ?"
  results <- quickQuery' conn sql [ toSql instanceID ]
  return $ toRegex <$> results
  where
    toRegex row =
      case row of
        (regexText:[]) ->
          case RGXT.compile defaultCompOpt (defaultExecOpt { captureGroups = False }) (fromSql regexText) of
            Left err -> error $ "!!!ERROR Parsing LogOmissionRegex: " ++ (fromSql regexText) ++ "\n" ++ err
            Right rgx -> rgx
        _ -> error $ "Invalid result from getLogOmissionRegexes: " ++ show row


--------------------------------------------------------------------------------
-- Wraps a query in a transaction, committing on success and rolling back on
-- error
--------------------------------------------------------------------------------
withConn :: Config -> (Connection -> IO a) -> IO a
withConn config f = do
  conn <- connectODBC (logShippersConnString config)
  result <- withTransaction conn f
  disconnect conn
  return result
