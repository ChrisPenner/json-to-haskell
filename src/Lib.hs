{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.Aeson
import Data.Aeson.Extra.Recursive
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable

data Struct =
        SArray Struct
      | SRecord (HM.HashMap T.Text Struct)
      | SMap Struct
      | SBool
      | SNumber
      | SNull
      | SString
      | SValue
      | SOptional Struct
      deriving (Show, Eq)
    -- It's possible it's a sum of multiple possible types
    --  | SSum [Struct]


-- Need to track the structs we "invent" along the way
analyze :: Value -> Struct
analyze = cata alg
  where
    alg :: ValueF Struct -> Struct
    alg = \case
      ObjectF m -> SRecord (HM.alter (fmap SMap) "*" m)
      ArrayF v -> case (v V.!? 0) of
          Just a -> SArray a
          Nothing -> SArray SValue
      StringF _ -> SString
      NumberF _ -> SBool
      BoolF _ -> SBool
      NullF -> SNull

parens :: MonadWriter T.Text m => m a -> m a
parens m = 
    tell "(" *> m <* tell ")"

line :: (MonadReader Int m, MonadWriter T.Text m) => m a -> m a
line m = do
    n <- ask
    tell $ T.replicate n " "
    m

builder :: Struct -> ReaderT Int (Writer T.Text) ()
builder struct =
    case struct of
        SNull -> tell "()"
        SString -> tell "Text"
        SNumber -> tell "Double"
        SBool -> tell "Bool"
        SValue -> tell "Value"
        SMap s -> tell "Map Text " >> parens (builder s)
        SOptional s -> tell "Maybe " >> parens (builder s)
        SArray s -> tell "Vector " >> parens (builder s)
        SRecord ss ->
            for_ (HM.toList ss) $ \(k, s) -> do
                line $ do
                    tell (k <> " :: ")
                    builder s



