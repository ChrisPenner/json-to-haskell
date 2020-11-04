{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Lib where

import Data.Aeson
import Data.Aeson.Extra.Recursive
import Data.Functor.Foldable hiding (fold)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable
import qualified Data.Set as S
import Text.Casing

indentation :: Int
indentation = 2

type StructName = T.Text
data RecordType = Ref | Structure
data Struct (r :: RecordType) where
        SArray :: Struct r -> Struct r
        SRecord :: (HM.HashMap T.Text (Struct 'Structure)) -> Struct 'Structure
        SRecordRef :: StructName -> Struct 'Ref
        SMap :: Struct r -> Struct r
        SBool :: Struct r
        SNumber :: Struct r
        SNull :: Struct r
        SString :: Struct r
        SValue :: Struct r
    -- It's possible it's a sum of multiple possible types
    --  | SSum [Struct]
        -- SOptional ::  Struct
deriving instance Show (Struct r)
deriving instance Eq (Struct r)
deriving instance Ord (Struct r)


-- data Blah = Blah { one :: a
--                  ,
--                  }

type RecordRep r = HM.HashMap T.Text (Struct r)
-- Need to track the structs we "invent" along the way
analyze :: Value -> Struct 'Structure
analyze = cata alg
  where
    alg :: ValueF (Struct 'Structure) -> Struct 'Structure
    alg = \case
      ObjectF m -> SRecord m
      ArrayF v -> case (v V.!? 0) of
          Just s -> SArray s
          Nothing -> SArray SValue
      StringF _ -> SString
      NumberF _ -> SNumber
      BoolF _ -> SBool
      NullF -> SNull

type Normalizer a = Writer (HM.HashMap T.Text (S.Set (HM.HashMap T.Text (Struct 'Ref)))) a

json2Haskell :: Value -> T.Text
json2Haskell v = do
    let struct = analyze v
        allStructs = execWriter $ normalize (nameRecord "model") struct
     in buildAllStructs allStructs

nameRecord :: T.Text -> RecordRep 'Ref -> Normalizer T.Text
nameRecord (toRecordName -> name) record = do
    tell . (HM.singleton name) . S.singleton $ record
    return name


toRecordName :: T.Text -> T.Text
toRecordName = T.pack . toPascal . fromAny . T.unpack

toFieldName :: T.Text -> T.Text
toFieldName = T.pack . toCamel . fromAny . T.unpack


normalize :: (RecordRep 'Ref -> Normalizer T.Text) -> Struct 'Structure -> Normalizer (Struct 'Ref)
normalize register = \case
  SRecord m -> do
      m' <- flip HM.traverseWithKey m $ \k v -> do
          normalize (nameRecord k) v
      name <- register $ m'
      return $ SRecordRef name
  SArray s -> SArray <$> normalize register s
  SMap m -> do
      SMap <$> normalize register m
  SBool -> pure SBool
  SNumber -> pure SNumber
  SNull -> pure SNull
  SString -> pure SString
  SValue -> pure SValue

parens :: MonadWriter T.Text m => m a -> m a
parens m =
    tell "(" *> m <* tell ")"

line :: (MonadReader Int m, MonadWriter T.Text m) => m a -> m a
line m = do
    n <- ask
    tell $ T.replicate n " "
    a <- m
    newline
    return  a

newline :: MonadWriter T.Text m => m ()
newline = tell "\n"

indented :: (MonadReader Int m, MonadWriter T.Text m) => m a -> m a
indented = local (+indentation)

type Builder a = ReaderT Int (Writer T.Text) ()

buildRecordDef :: StructName -> HM.HashMap T.Text (Struct 'Ref) -> Builder ()
buildRecordDef name struct = do
    line . tell . fold $ ["data ", name, " = ", name]
    indented $ for_ (zip [0 :: Int ..] $ HM.toList struct) $ \(i, (k, v)) -> do
        line $ do
            if (i == 0) then tell "{ "
                        else tell ", "
            tell $ toFieldName k
            tell " :: "
            buildType v
    indented . line $ tell "}"

buildType :: Struct 'Ref -> Builder ()
buildType =
  \case
    SNull -> tell "()"
    SString -> tell "Text"
    SNumber -> tell "Double"
    SBool -> tell "Bool"
    SValue -> tell "Value"
    SMap s -> tell "Map Text " >> parens (buildType s)
    SArray s -> tell "Vector " >> parens (buildType s)
    SRecordRef n -> tell n
    -- SOptional s -> tell "Maybe " >> parens (builder s)
    -- SRecord ss ->
    --     for_ (HM.toList ss) $ \(k, s) -> do
    --         line $ do
    --             tell (k <> " :: ")
    --             builder s



buildAllStructs :: HM.HashMap T.Text (S.Set (RecordRep 'Ref)) -> T.Text
buildAllStructs hm = execWriter . flip runReaderT 0 $ do
    flip HM.traverseWithKey hm $ \k v -> do
        buildRecordDef k (head . S.toList $ v)
        newline
