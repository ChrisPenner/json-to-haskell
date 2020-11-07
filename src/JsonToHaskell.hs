{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module JsonToHaskell
    ( json2Haskell
    , Options(..)
    , defaultOptions
    ) where

import JsonToHaskell.Internal.Options
import JsonToHaskell.Internal.Printer
import JsonToHaskell.Internal.Parser
import Data.Aeson (Value)
import qualified Data.Text as T
import qualified Data.Bimap as BM

json2Haskell :: Options -> Value -> T.Text
json2Haskell opts v = do
    let allStructs = analyze v
        namedStructs = canonicalizeRecordNames allStructs
        referencedStructs = BM.mapR (fmap (dereference namedStructs)) namedStructs
     in buildAllStructs opts referencedStructs
