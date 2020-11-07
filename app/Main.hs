{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import JsonToHaskell
import Data.Aeson hiding (defaultOptions)
import Text.RawString.QQ (r)
import Data.Text.IO as T
import Data.ByteString.Lazy as BS

value :: Either String Value
value = eitherDecode valueStr

valueStr :: BS.ByteString
valueStr = [r|
{
  "name": "jon",
  "age and stuff": 37,
  "is-employed": true,
  "\"pets_maybe": [["Garfield"], ["Odie"]],
  "address": {
    "street": "221B",
    "zip": 12345,
    "other" : {
      "one": 1
    }
  },
  "other-address": {
    "street": "221B",
    "zip2": 12345,
    "other" : {
      "two": [{}]
    }
  }
}
|]

main :: IO ()
main = do
    v <- either fail pure value
    T.putStrLn $ json2Haskell defaultOptions v
