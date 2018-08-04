{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Exception
import Data.Text
import Data.Text.Read
import System.Environment
import System.IO.Error

data Config = Config
  { dbName :: Text
  , dbHost :: Text
  , dbPort :: Int
  , dbUser :: Text
  , dbPass :: Text
  , weatherAPIKey :: Text
  , mailFrom :: Text
  , mailTo :: [Text]
  , mailBody :: Text
  , smtpHost :: Text
  , smtpUser :: Text
  , smtpPass :: Text
  } deriving (Show)

tGetEnv :: String -> IO Text
tGetEnv a = pack <$> getEnv a

safeTGetEnv :: String -> Text -> IO Text
safeTGetEnv n def =
  catch (tGetEnv n) (\e ->
    if isDoesNotExistError e
    then return def  
    else ioError e)

readInt :: Int -> Text -> Int
readInt d t =
  let e = decimal t
  in either (const d) fst e

config :: IO Config
config = Config
           <$>
           safeTGetEnv "BALCONY_DB_NAME" "balcony"
           <*>
           safeTGetEnv "BALCONY_DB_HOST" "localhost"
           <*>
           fmap (readInt 5432) (tGetEnv "BALCONY_DB_PORT")
           <*>
           safeTGetEnv "BALCONY_DB_USER" "balcony"
           <*>
           safeTGetEnv "BALCONY_DB_PASS" ""
           <*>
           tGetEnv "BALCONY_WEATHER_API_KEY"
           <*>
           tGetEnv "BALCONY_MAIL_FROM"
           <*>
           fmap (splitOn ",") (tGetEnv "BALCONY_MAIL_TO")
           <*>
           safeTGetEnv "BALCONY_MAIL_BODY"
                      "Please water the balcony tonight. The average temperature between 9AM and 7PM was {{avg}} degrees Celcius."
           <*>
           safeTGetEnv "BALCONY_SMTP_HOST" "smtp.gmail.com"
           <*>
           tGetEnv "BALCONY_SMTP_USER"
           <*>
           tGetEnv "BALCONY_SMTP_PASS"
