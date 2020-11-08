{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import JsonToHaskell
import Data.Aeson hiding (defaultOptions)
import Text.RawString.QQ (r)
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import System.Exit
import Flags

main :: IO ()
main = do
    opts <- execParser optionsParserInfo
    input <- BL.getContents
    case eitherDecode input of
        Left err -> putStrLn err >> exitWith (ExitFailure 1)
        Right val -> T.putStrLn $ jsonToHaskell opts val

value :: Either String Value
value = eitherDecode valueStr

valueStr :: BL.ByteString
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

-- main :: IO ()
-- main = do
--     v <- either fail pure value
--     T.putStrLn $ jsonToHaskell performantOptions v

