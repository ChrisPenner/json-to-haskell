{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Lib
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.State
import Text.RawString.QQ (r)
import Data.Text.IO as T

value :: Value
value = view (singular (_JSON @String)) ([r|
{
  "name": "jon",
  "age": 37,
  "employed": true,
  "pets": ["Garfield", "Odie"],
  "address": {
    "street": "221B",
    "zip": 12345
  }
}
|])

main :: IO ()
main = do
    T.putStrLn $ json2Haskell value
    -- putStrLn "Type"
    -- print a
    -- putStrLn ""
    -- putStrLn "SavedRecords"
    -- print b

