{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Lib
import Data.Aeson hiding (defaultOptions)
import Text.RawString.QQ (r)
import Data.Text.IO as T
import Data.Maybe

value :: Value
value = fromJust $ decode ([r|
{
  "name": "jon",
  "age and stuff": 37,
  "employed": true,
  "pets": [["Garfield"], ["Odie"]],
  "address": {
    "street": "221B",
    "zip": 12345
  },
  "other-address": {
    "street": "221B",
    "zip2": 12345
  }
}
|])

main :: IO ()
main = do
    T.putStrLn $ json2Haskell defaultOptions value
    -- putStrLn "Type"
    -- print a
    -- putStrLn ""
    -- putStrLn "SavedRecords"
    -- print b

