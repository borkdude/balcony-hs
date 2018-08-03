{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lib
    ( sendAvgToday
    , storeWeather
    , config
    ) where

import           Config
import           Control.Lens
import           Data.Aeson (FromJSON, parseJSON, withObject, eitherDecode, (.:))
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (ByteString)
import           Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as T (Text, unpack, pack, replace)
import           Data.Time.Calendar (addDays)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           Database.PostgreSQL.Simple
import           GHC.Int
import           Mail
import           Network.Wreq
import           Text.Printf (printf)

connectionString :: Config -> String
connectionString Config {..} = printf
  "host='%s' port=%d user='%s' password='%s' dbname='%s'"
  dbHost dbPort dbUser dbPass dbName

newtype Weather = Weather { temperature :: Float }
  deriving (Show)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v -> Weather
    <$> v .: "temp"

newtype WeatherResponse = WeatherResponse [Weather]

instance FromJSON WeatherResponse where
  parseJSON = withObject "WeatherResponse" $ \v -> WeatherResponse
    <$> v .: "data"

getWeather :: String -> IO ByteString
getWeather apiKey = do
  let urlTemplate = "https://api.weatherbit.io/v2.0/current?city=Amersfoort,NL&key=%s"
      url = printf urlTemplate apiKey
  response <- get url
  return $ response  ^. responseBody

storeWeather' :: String -> ByteString -> IO GHC.Int.Int64  
storeWeather' connString weather = do
  let parsed = eitherDecode weather :: Either String WeatherResponse
      q = "insert into weather (response, temp) values (?, ?)"
  case parsed of
    Right (WeatherResponse pl) ->
      case pl of
        [Weather temp] -> do
          conn <- connectPostgreSQL $ BC.pack connString
          execute conn q (weather, temp)
        xs -> error $ "unexpected result" ++ show xs  
    Left err -> error err

storeWeather :: Config -> IO Int64
storeWeather = do
  connString <- connectionString
  apiKey <- weatherAPIKey
  return $ getWeather apiKey >>= storeWeather' connString

getAvgToday :: String -> IO (Maybe Float)
getAvgToday connString = do
  today <- utctDay <$> getCurrentTime
  let tempQuery = "select avg(temp) from weather where _created >= ? and _created <= ?"
      tomorrow = addDays 1 today
  conn <- connectPostgreSQL $ BC.pack connString
  res <- query conn tempQuery (show today, show tomorrow) :: IO [Only (Maybe Scientific)]
  return $ case res of
    [Only m] -> case m of
      Nothing -> Nothing
      Just avg -> Just (toRealFloat avg)
    _ -> Nothing

mailText :: Config
         -> Float -> T.Text
mailText = do
  body <- mailBody
  return $ \avg -> let
    bt = T.pack body
    prettyTemp = T.pack $ printf "%2.1f" avg
    t = T.replace "{{avg}}" prettyTemp bt
    in t

sendAvgToday :: Config -> IO ()
sendAvgToday = do
  simpleMail <- sendSimpleMail
  connString <- connectionString
  bodyTemplate <- mailText
  return $ do
    avg <- getAvgToday connString
    case avg of
      Nothing -> putStrLn "No temperatures recorded for today yet."
      Just temp ->
        let body = bodyTemplate temp
        in putStrLn (T.unpack body) >> simpleMail body
