module Mail where

import Control.Monad
import Data.ByteString.Lazy (toStrict)
import Data.Text as T (Text, pack, unpack)
import Data.Text.Lazy as L (Text, pack, fromStrict)
import Network.HaskellNet.Auth
import qualified Network.HaskellNet.SMTP as S
import qualified Network.HaskellNet.SMTP.SSL as SSL
import Network.Mail.Mime as M
import Config as C

-- thanks to http://czyzykowski.com/posts/ssl-email-haskell.html

toString :: Address -> String
toString Address { addressEmail = email } = unpack email

sendMail :: String -> String -> String -> Mail -> IO ()
sendMail smtpHost smtpUser smtpPass msg = do
  rendered   <- renderMail' msg
  SSL.doSMTPSSL smtpHost $ \connection -> do
      succeeded  <- SSL.authenticate
                      LOGIN
                      smtpUser
                      smtpPass
                      connection
      when succeeded $
          SSL.sendMail (toString (M.mailFrom msg))
                       (map toString (M.mailTo msg))
                       (toStrict rendered) connection

sendSimpleMail ::
  Config -> T.Text -> IO ()

sendSimpleMail c text =
  let mail = Mail
        (Address Nothing (T.pack (C.mailFrom c)))
        (map (Address Nothing . T.pack) (C.mailTo c))
        []
        []
        []
        [[plainPart $ L.fromStrict text]]
  in sendMail (smtpHost c) (smtpUser c) (smtpPass c) mail
