{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DwLog where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Array           ((!))
import           Data.Char            (toLower, toUpper)
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           GHC.Exts             (fromList)
import           Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as RGXT

data DwLogMeta = DwLogMeta
  { client      :: Text
  , filename    :: Text
  , environment :: Text
  } deriving (Eq, Show)

data DwLog = DwLog
  { logmeta                :: DwLogMeta
  , timestamp              :: LocalTime
  , logType                :: LogType
  , level                  :: Text
  , servlet                :: Text
  , site                   :: Text
  , pipeline               :: Text
  , caller                 :: Text
  , sessionID              :: Text
  , sessionType            :: Maybe Text
  , requestID              :: Maybe Text
  , logger                 :: Text
  , message                :: Text
  , systemInfo             :: [(Text, Text)]
  , requestInfo            :: [(Text, Text)]
  , requestParams          :: [(Text, Text)]
  , malformedRequestParams :: Maybe Text
  , strayHttpHeaders       :: Maybe Text
  , stackTrace             :: Maybe StackTrace
  , jobName                :: Maybe Text
  , quotaInfo              :: Maybe QuotaInfo
  , deprecation            :: Maybe DeprecationInfo
  , apiInfo                :: Maybe ApiInfo
  } deriving (Eq, Show)

emptyDwLog :: DwLogMeta -> LocalTime -> LogType -> DwLog
emptyDwLog logmeta' timestamp' logType' =
  DwLog
    { logmeta = logmeta'
    , timestamp = timestamp'
    , logType = logType'
    , level = ""
    , servlet = ""
    , site = ""
    , pipeline = ""
    , caller = ""
    , sessionID = ""
    , sessionType = Nothing
    , requestID = Nothing
    , logger = ""
    , message = ""
    , systemInfo = []
    , requestInfo = []
    , requestParams = []
    , malformedRequestParams = Nothing
    , strayHttpHeaders = Nothing
    , stackTrace = Nothing
    , jobName = Nothing
    , quotaInfo = Nothing
    , deprecation = Nothing
    , apiInfo = Nothing
    }

data LogType
  = CustomLog
  | SystemLog
  | QuotaLog
  | ApiLog
  | ApiDeprecationLog
  deriving (Eq, Show)

data StackTrace
  = StackTrace StackTraceID Text
  | StackTraceRef StackTraceID
  deriving (Eq, Show)

type StackTraceID = Text

data QuotaInfo = QuotaInfo
  { warn          :: Maybe Int
  , limit         :: Int
  , exceededTimes :: Int
  , maxActual     :: Int
  , quotaType     :: Text
  } deriving (Eq, Show)

data DeprecationInfo = DeprecationInfo
  { scriptMethod :: Maybe Text
  , decisionNode :: Maybe Text
  , depWhere     :: Text
  , times        :: Int
  } deriving (Eq, Show)

emptyDeprecationInfo :: DeprecationInfo
emptyDeprecationInfo = DeprecationInfo
  { scriptMethod = Nothing
  , decisionNode = Nothing
  , depWhere = ""
  , times = 0
  }

data ApiInfo = ApiInfo
  { violation :: Maybe Text
  , method    :: Maybe Text
  } deriving (Eq, Show)

emptyApiInfo :: ApiInfo
emptyApiInfo = ApiInfo
  { violation = Nothing
  , method = Nothing
  }

--------------------------------------------------------------------------------
-- JSON encoding
--------------------------------------------------------------------------------
instance ToJSON LogType where
  toJSON CustomLog         = String "dw-custom"
  toJSON SystemLog         = String "dw-system"
  toJSON QuotaLog          = String "dw-quota"
  toJSON ApiLog            = String "dw-api"
  toJSON ApiDeprecationLog = String "dw-deprecation"

instance ToJSON QuotaInfo where
  toJSON q = objectOmitNullAndEmpties
    [ "warn" .= warn q
    , "limit" .= limit q
    , "exceededTimes" .= exceededTimes q
    , "maxActual" .= maxActual q
    , "quotaType" .= quotaType q
    ]

instance ToJSON DwLog where
  toJSON l = objectOmitNullAndEmpties $
    [ "@timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (timestamp l)
    , "client" .= T.toLower (client $ logmeta l)
    , "logFile" .= filename (logmeta l)
    , "type" .= logType l
    , "level" .= level l
    , "servlet" .= servlet l
    , "site" .= site l
    , "pipeline" .= pipeline l
    , "caller" .= caller l
    , "sessionID" .= sessionID l
    , "sessionType" .= sessionType l
    , "requestID" .= requestID l
    , "logger" .= logger l
    , "message" .= message l
    , "systemInfo" .= keyValPairsToJSON (Just systemInfoKeyOmissionRegex) (systemInfo l)
    , "requestInfo" .= keyValPairsToJSON Nothing (requestInfo l)
    , "requestParams" .= keyValPairsToJSON Nothing (getFirstClassRequestParams $ requestParams l)
    , "requestParamsAll" .= keyValPairsToJSONExplicit (requestParams l)
    , "malformedRequestParams" .= malformedRequestParams l
    , "strayHttpHeaders" .= strayHttpHeaders l
    , "jobName" .= jobName l
    , "quota" .= quotaInfo l
    , "deprecation" .= deprecation l
    , "api" .= apiInfo l

    -- logstash won't let you override 'host', so call it 'server'
    , "server" .= parseHost (filename $ logmeta l)

    , "environment" .= (environment . logmeta) l
    ] ++ (stackTraceToJSON . stackTrace) l
    where
      stackTraceToJSON Nothing = []
      stackTraceToJSON (Just (StackTrace stID s)) =
        [ "stackTraceID" .= stID
        , "stackTrace" .= s
        ]
      stackTraceToJSON (Just (StackTraceRef stID)) =
        [ "stackTraceID" .= stID
        ]

instance ToJSON DeprecationInfo where
  toJSON d = objectOmitNullAndEmpties $
    [ "scriptMethod" .= scriptMethod d
    , "decisionNode" .= decisionNode d
    , "where" .= depWhere d
    , "times" .= times d
    ]

instance ToJSON ApiInfo where
  toJSON i = objectOmitNullAndEmpties $
    [ "violation" .= violation i
    , "method" .= method i
    ]

keyValPairsToJSON :: Maybe Regex -> [(Text, Text)] -> Value
keyValPairsToJSON _ [] = Null
keyValPairsToJSON omitKeysRegex list =
  Object $ fromList $ filter omit $ fmap f list
  where
    f (k, v) = toJsonCase k .= v
    omit (k, _) =
      case omitKeysRegex of
        Nothing -> True
        Just rgx ->
          case RGXT.execute rgx k of
            Right (Just _) -> False
            _              -> True

keyValPairsToJSONExplicit :: [(Text, Text)] -> Value
keyValPairsToJSONExplicit [] = Null
keyValPairsToJSONExplicit list = Array $ fromList $ fmap f list
  where
    f (k, v) = object [ "key" .= k, "value" .= v ]

-- Pulls out request parameters that we want to promote to first-class citizens in the json
getFirstClassRequestParams :: [(Text, Text)] -> [(Text, Text)]
getFirstClassRequestParams =
  let
    keyMap "pid"       = Just "productID"
    keyMap "productID" = Just "productID"
    keyMap "cgid"      = Just "categoryID"
    keyMap "caid"      = Just "contentAssetID"
    keyMap "format"    = Just "format"
    keyMap "source"    = Just "source"
    keyMap _           = Nothing
    f (key, val) = (\newKey -> (newKey, val)) <$> keyMap key
  in
    -- the arbitrary sorting on key length makes "pid" take precedence over "productID"
    nubBy (\(a,_) (b,_) -> a == b) . mapMaybe f . sortBy (comparing (T.length . fst))

objectOmitNullAndEmpties :: [Pair] -> Value
objectOmitNullAndEmpties = object . Prelude.filter f
  where
    f (_, Null)     = False
    f (_, String s) = not $ T.null s
    f _             = True

toJsonCase :: Text -> Text
toJsonCase key =
  let
    mapHead f txt =
      if T.null txt
        then ""
        else f (T.head txt) `T.cons` T.tail txt
  in
    if T.toUpper key == key && isNothing (T.find (== ' ') key)
      then T.toLower key
      else mapHead Data.Char.toLower $ T.concat $ fmap (mapHead Data.Char.toUpper) $ T.split (== ' ') key

parseHost :: Text -> Maybe Text
parseHost fileName =
  case RGXT.execute bladeRegex fileName of
    Right (Just matches) ->
      case length matches of
        2 ->
          let (matchStart, matchLength) = matches ! 1
          in Just $ T.take matchLength $ T.drop matchStart fileName
        _ -> Nothing
    _ -> Nothing

bladeRegex :: Regex
bladeRegex =
  case RGXT.compile defaultCompOpt defaultExecOpt "-(blade[0-9]+-[0-9]+)-" of
    Left err  -> error err
    Right rgx -> rgx

systemInfoKeyOmissionRegex :: Regex
systemInfoKeyOmissionRegex =
  case RGXT.compile defaultCompOpt (defaultExecOpt { captureGroups = False }) "^(requestID|sessionType|sessionID)$" of
    Left err  -> error err
    Right rgx -> rgx
