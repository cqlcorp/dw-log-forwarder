{-# LANGUAGE OverloadedStrings #-}
module DwLog.LogParsing.WebDav
  ( WebDavEnvironment(..)
  , DwLogFolder(..)
  , ServerLogFile(..)
  , getFolderContents
  , filterFolderContents
  , tailLogFile
  , downloadFile
  ) where

import qualified Control.Exception             as E
import           Control.Lens
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy.Internal as BSLI
import           Data.Either
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Time
import           Network.HTTP.Client           (HttpException (..))
import           Network.Wreq
import           Text.Parsec
import           Text.Parsec.Text

data WebDavEnvironment = WebDavEnvironment
  { server   :: Text
  , username :: Text
  , password :: Text
  }

data DwLogFolder
  = Root
  | Deprecation
  | LogArchive
  deriving (Eq, Show)

data ServerLogFile = ServerLogFile
  { filePath              :: Text
  , utcServerLastModified :: LocalTime
  } deriving (Show)

--------------------------------------------------------------------------------
-- Given a webdav folder, this lists the contents. It's based off a strict
-- format of the webdav listing given by Apache, so any future change to that
-- can easily break the parsing
--------------------------------------------------------------------------------
getFolderContents :: WebDavEnvironment -> DwLogFolder -> IO (Either Text [ServerLogFile])
getFolderContents env folder = do
  let opts = defaults & auth ?~ basicAuth (TE.encodeUtf8 $ username env) (TE.encodeUtf8 $ password env)
  let url = T.unpack $ dwLogFolder env folder
  r <- getWith opts url
  case r ^. responseStatus . statusCode of
    200 -> do
      let body = TLE.decodeUtf8 $ r ^. responseBody
      return $ Right $ parseFilesFromWebDavFolder body
    _ -> return $ Left $ T.concat ["Could not get WebDAV ", T.pack $ show folder, " folder contents. Response status: ", T.pack $ show (r ^. responseStatus)]

--------------------------------------------------------------------------------
-- Same as getFolderContents but you can filter the files
--------------------------------------------------------------------------------
filterFolderContents :: WebDavEnvironment -> DwLogFolder -> (ServerLogFile -> Bool) -> IO (Either Text [ServerLogFile])
filterFolderContents env folder condition = do
  everything <- getFolderContents env folder
  return $ (filter condition) <$> everything

--------------------------------------------------------------------------------
-- Given an environment and log folder type, this gives you the url
--------------------------------------------------------------------------------
dwLogFolder :: WebDavEnvironment -> DwLogFolder -> Text
dwLogFolder env folder =
  let root = T.concat ["https://", server env, "/on/demandware.servlet/webdav/Sites/Logs"]
  in
    case folder of
      Root        -> root
      Deprecation -> T.concat [root, "/deprecation"]
      LogArchive  -> T.concat [root, "/log_archive"]

--------------------------------------------------------------------------------
-- This is the simple log file list parser given Apache's file listing output
--------------------------------------------------------------------------------
parseFilesFromWebDavFolder :: TL.Text -> [ServerLogFile]
parseFilesFromWebDavFolder body =
  let
    split = T.splitOn "</tr>" $ TL.toStrict body
    parseChunk = runP serverLogFileParser () ""
  in rights (parseChunk <$> split)

serverLogFileParser :: Parser ServerLogFile
serverLogFileParser = do
  let tillString = manyTill anyChar . try . string
  discard $ tillString "href=\""
  filePath' <- tillString "\""
  discard $ tillString "<td"
  discard $ tillString "<tt>"
  size <- tillString "</tt>"

  if size == "0.0 kb"
    then parserFail "Empty file"
    else return ()

  discard $ tillString "<tt>"

  timeStr <- tillString "</tt>"

  case parseTimeM True defaultTimeLocale rfc822DateFormat timeStr of
    Nothing -> parserFail "Invalid last modified date"
    Just t  -> return $ ServerLogFile (T.pack filePath') t

--------------------------------------------------------------------------------
-- Returns the tail end of a log file starting at `sinceBytes`. The tuple
-- returned on success contains the number of bytes read and the text body
-- stream.
--------------------------------------------------------------------------------
tailLogFile :: WebDavEnvironment -> Integer -> Text -> IO (Either Text (Integer, TL.Text))
tailLogFile env sinceBytes filePath' = do
  let
    range = BS8.pack $ concat ["bytes=", show sinceBytes, "-"]
    opts = defaults
      & auth ?~ basicAuth (TE.encodeUtf8 $ username env) (TE.encodeUtf8 $ password env)
      & header "Range" .~ [range]
  let url = concat ["https://", T.unpack $ server env, T.unpack filePath']
  resp <- (Just <$> getWith opts url) `E.catch` handler
  case resp of
    Nothing -> return $ Right (0, TL.empty)
    Just r -> do
      let rangeHeader = TE.decodeUtf8 (r ^. responseHeader "Content-Range")
      let bytesReceived = runP bytesReceivedParser () "" rangeHeader
      case bytesReceived of
        Left _ -> return $ Left $ T.pack $ "Invalid range header: " ++ T.unpack rangeHeader
        Right (br, totalBytes) ->
          if br + sinceBytes /= totalBytes
            then return $ Left $ T.concat ["Mismatch between starting byte position (", T.pack $ show sinceBytes, "), bytes received (", T.pack $ show br, ") and total bytes (", T.pack $ show totalBytes, ")"]
            else return $ Right (br, TLE.decodeUtf8 $ r ^. responseBody)
  where
    handler e@(StatusCodeException s _ _ )
      | s ^. statusCode == 416 = return Nothing
      | otherwise = E.throwIO e
    handler e = E.throwIO e

--------------------------------------------------------------------------------
-- Download a file to a stream
--------------------------------------------------------------------------------
downloadFile :: WebDavEnvironment -> Text -> IO (Either Text BSLI.ByteString)
downloadFile env filePath' = do
  let
    opts = defaults
      & auth ?~ basicAuth (TE.encodeUtf8 $ username env) (TE.encodeUtf8 $ password env)
  let url = concat ["https://", T.unpack $ server env, T.unpack filePath']
  resp <- getWith opts url
  return $ Right $ resp ^. responseBody

--------------------------------------------------------------------------------
-- Parses 'bytes 0-99/100'
-- Returns (the number of bytes read, total bytes read)
--------------------------------------------------------------------------------
bytesReceivedParser :: Parser (Integer, Integer)
bytesReceivedParser = do
  discard $ string "bytes "
  start <- many1 digit
  discard $ char '-'
  end <- many1 digit
  discard $ char '/'
  total <- many1 digit
  return $ (((read end :: Integer) - (read start :: Integer) + 1) , read total)

-- so ghc doesn't force _ <- whatever
discard :: Monad m => m a -> m ()
discard m = m >> return ()
