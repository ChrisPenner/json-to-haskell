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
    ( jsonToHaskell
    , simpleOptions
    , performantOptions
    , Options(..)
    , NumberType(..)
    , TextType(..)
    , MapType(..)
    , ListType(..)
    ) where

import JsonToHaskell.Internal.Options
import JsonToHaskell.Internal.Printer
import JsonToHaskell.Internal.Parser
import Data.Aeson (Value)
import qualified Data.Text as T
import qualified Data.Bimap as BM

-- | Transform an Aeson 'Value' into a complete Haskell module (as 'T.Text').
-- This function is all that you need.
--
-- Use 'defaultOptions' as a reasonable starting point.
jsonToHaskell :: Options -> Value -> T.Text
jsonToHaskell opts v = do
    let allStructs = analyze v
        namedStructs = canonicalizeRecordNames allStructs
        referencedStructs = BM.mapR (fmap (addReferences namedStructs)) namedStructs
     in T.strip $ writeModel opts referencedStructs
