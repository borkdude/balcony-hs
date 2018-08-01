module Main where

import Lib
import System.Environment
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  c <- config
  case args of
    ["log"] -> void $ storeWeather c
    ["mail"] -> void $ sendAvgToday c
    _ -> putStrLn "use log or mail command"
