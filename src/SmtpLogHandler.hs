{-# LANGUAGE OverloadedStrings #-}
module SmtpLogHandler where

import qualified Control.Exception         as Ex
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Network.Mail.SMTP
import           System.IO
import           System.Log
import           System.Log.Formatter
import           System.Log.Handler.Simple

import           Config

smtpLogHandler :: DiagnosticsInfo -> Priority -> IO (GenericHandler DiagnosticsInfo)
smtpLogHandler cfg pri = do
  return $
    GenericHandler
      { priority = pri
      , formatter = nullFormatter
      , privData = cfg
      , writeFunc = sendEmailLog
      , closeFunc = \_ -> return ()
      }
  where
    sendEmailLog cfg' msg = do
      let from = Address Nothing $ T.pack $ fromEmail cfg'
      let to = Address Nothing . T.pack <$> diagnosticsEmails cfg'
      let subject = "dw-log-forwarder Log"
      let body = plainTextPart $ TL.pack msg

      Ex.handle handler $
        sendMail (smtpServer cfg') $ simpleMail from to [] [] subject [body]
    handler ex = do
      hPutStrLn stderr $ show (ex :: Ex.SomeException)

