module Config where

import System.Environment
import Control.Exception
import System.IO.Error
import Data.List.Split

data Config = Config
  { dbName :: String
  , dbHost :: String
  , dbPort :: Int
  , dbUser :: String
  , dbPass :: String
  , weatherAPIKey :: String
  , mailFrom :: String
  , mailTo :: [String]
  , mailBody :: String
  , smtpHost :: String
  , smtpUser :: String
  , smtpPass :: String
  } deriving (Show)

safeGetEnv n def =
  catch (getEnv n)
    (\e ->
       if isDoesNotExistError e
       then return def  
       else ioError e)

config = Config
           <$>
           safeGetEnv "BALCONY_DB_NAME" "balcony"
           <*>
           safeGetEnv "BALCONY_DB_HOST" "localhost"
           <*>
           fmap read (safeGetEnv "BALCONY_DB_PORT" "5432")
           <*>
           safeGetEnv "BALCONY_DB_USER" "balcony"
           <*>
           safeGetEnv "BALCONY_DB_PASS" ""
           <*>
           getEnv "BALCONY_WEATHER_API_KEY"
           <*>
           getEnv "BALCONY_MAIL_FROM"
           <*>
           fmap (splitOn ",") (getEnv "BALCONY_MAIL_TO")
           <*>
           safeGetEnv "BALCONY_MAIL_BODY"
                      "Please water the balcony tonight. The average temperature between 9AM and 7PM was {{avg}} degrees Celcius."
           <*>
           safeGetEnv "BALCONY_SMTP_HOST" "smtp.gmail.com"
           <*>
           getEnv "BALCONY_SMTP_USER"
           <*>
           getEnv "BALCONY_SMTP_PASS"
