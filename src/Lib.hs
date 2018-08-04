{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lib
    ( sendAvgToday
    , storeWeather
    , config
    ) where

import           Config
import           Control.Lens
import           Data.Aeson (FromJSON, parseJSON, withObject, eitherDecode, (.:))
import qualified Data.ByteString as B (ByteString)
import           Data.ByteString.Lazy (ByteString)
import           Data.Either (either)
import           Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as T (Text, pack, replace)
import           Data.Text.Encoding
import           Data.Time.Calendar (addDays)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           Database.PostgreSQL.Simple
import           Formatting
import           GHC.Int
import           Mail
import           Network.Wreq
import           Text.Printf (printf)

type ConnectionString = B.ByteString

connectionString :: Config -> ConnectionString
connectionString Config {..} =
  let cs = sformat ("host=" % stext % " port=" % int % " user=" % stext %
                   " password=" % stext % " dbname=" % stext)
           dbHost dbPort dbUser dbPass dbName
  in encodeUtf8 cs

newtype Weather = Weather { temperature :: Float }
  deriving (Show)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v -> Weather
    <$> v .: "temp"

newtype WeatherResponse = WeatherResponse [Weather]

instance FromJSON WeatherResponse where
  parseJSON = withObject "WeatherResponse" $ \v -> WeatherResponse
    <$> v .: "data"

getWeather :: T.Text -> IO ByteString
getWeather apiKey = do
  let urlTemplate = "https://api.weatherbit.io/v2.0/current?city=Amersfoort,NL&key=%s"
      url = printf urlTemplate apiKey
  response <- get url
  return $ response  ^. responseBody

storeWeather' :: ConnectionString -> ByteString -> IO GHC.Int.Int64  
storeWeather' connString weather = do
  let parsed = eitherDecode weather :: Either String WeatherResponse
  either error store parsed where
    store (WeatherResponse pl) =
      let q = "insert into weather (response, temp) values (?, ?)"
      in
        case pl of
          [Weather temp] -> do
            conn <- connectPostgreSQL connString
            execute conn q (weather, temp)
          xs -> error $ "unexpected result" ++ show xs  

storeWeather :: Config -> IO Int64
storeWeather = do
  connString <- connectionString
  apiKey <- weatherAPIKey
  return $ getWeather apiKey >>= storeWeather' connString

getAvgToday :: ConnectionString -> IO (Maybe Float)
getAvgToday connString = do
  today <- utctDay <$> getCurrentTime
  let tempQuery = "select avg(temp) from weather where _created >= ? and _created <= ?"
      tomorrow = addDays 1 today
  conn <- connectPostgreSQL connString
  res <- query conn tempQuery (show today, show tomorrow) :: IO [Only (Maybe Scientific)]
  return $ case res of
    [Only m] -> fmap toRealFloat m
    _ -> Nothing

mailText :: T.Text
         -> Float -> T.Text
mailText template avg = let
  prettyTemp = T.pack $ printf "%2.1f" avg
  in T.replace "{{avg}}" prettyTemp template

sendAvgToday :: Config -> IO ()
sendAvgToday = do
  simpleMail <- sendSimpleMail
  connString <- connectionString
  body <- mailBody
  return $ do
    avg <- getAvgToday connString
    case avg of
      Nothing -> putStrLn "No temperatures recorded for today yet."
      Just temp ->
        let mt = mailText body temp
        in simpleMail mt
