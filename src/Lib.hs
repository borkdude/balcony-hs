{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as TL (pack, Text)
import           Data.Time.Calendar (addDays)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           Database.PostgreSQL.Simple
import           Mail
import           Network.Wreq
import           System.Environment (getEnv)
import           Text.Printf (printf)

connectionString :: Config -> String
connectionString c = printf
  "host='%s' port=%d user='%s' password='%s' dbname='%s'"
  (dbHost c) (dbPort c) (dbUser c) (dbPass c) (dbName c)

data Weather = Weather { temperature :: Float }
  deriving (Show)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v -> Weather
    <$> v .: "temp"

data WeatherResponse = WeatherResponse { payload :: [Weather] }

instance FromJSON WeatherResponse where
  parseJSON = withObject "WeatherResponse" $ \v -> WeatherResponse
    <$> v .: "data"

getWeather :: String -> IO ByteString
getWeather apiKey = do
  let urlTemplate = "https://api.weatherbit.io/v2.0/current?city=Amersfoort,NL&key=%s"
      url = printf urlTemplate apiKey
  response <- get url
  return $ response  ^. responseBody

storeWeather' connString weather = do
  let parsed = eitherDecode weather :: Either String WeatherResponse
      query = "insert into weather (response, temp) values (?, ?)"
  case parsed of
    Right (WeatherResponse pl) ->
      case pl of
        [] -> error "no data"
        [Weather temp] -> do
          conn <- connectPostgreSQL $ BC.pack connString
          execute conn query (weather, temp)
    Left err -> error err

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
  res <- query conn tempQuery (show today, show tomorrow) :: IO [Only Scientific]
  return $ case res of
    [] -> Nothing
    [Only m] -> Just (toRealFloat m)

sendAvgToday = do
  simpleMail <- sendSimpleMail
  connString <- connectionString
  return $ do
    avg <- getAvgToday connString
    case avg of
      Nothing -> putStrLn "No temperatures recorded for today yet."
      Just temp ->
        let prettyTemp = printf "%2.1f" temp
        in simpleMail prettyTemp
