{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Mail where

import           Config as C
import           Control.Monad
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text, unpack)
import           Data.Text.Lazy (fromStrict)
import           Network.HaskellNet.Auth
import qualified Network.HaskellNet.SMTP.SSL as SSL
import           Network.Mail.Mime as M

-- thanks to http://czyzykowski.com/posts/ssl-email-haskell.html

toString :: Address -> String
toString Address { addressEmail = email } = unpack email

sendMail :: Text -> Text -> Text -> Mail -> IO ()
sendMail host user pass msg = do
  rendered   <- renderMail' msg
  SSL.doSMTPSSL (unpack host) $ \connection -> do
      succeeded  <- SSL.authenticate
                      LOGIN
                      (unpack user)
                      (unpack pass)
                      connection
      when succeeded $
          SSL.sendMail (toString (M.mailFrom msg))
                       (map toString (M.mailTo msg))
                       (toStrict rendered) connection

sendSimpleMail ::
  Config -> Text -> IO ()
sendSimpleMail Config {..} text =
  let mail = Mail
        (Address Nothing mailFrom)
        (map (Address Nothing) mailTo)
        []
        []
        [("Subject", "Should I water my balcony?")]
        [[plainPart $ fromStrict text]]
  in sendMail smtpHost smtpUser smtpPass mail
