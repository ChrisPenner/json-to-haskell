{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import JsonToHaskell
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import System.Exit
import Flags
import Data.Aeson

main :: IO ()
main = do
    opts <- execParser optionsParserInfo
    input <- BL.getContents
    case eitherDecode input of
        Left err -> putStrLn err >> exitWith (ExitFailure 1)
        Right val -> T.putStrLn $ jsonToHaskell opts val
