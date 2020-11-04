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
module Lib where

import Data.Aeson hiding (Options)
import Data.Aeson.Extra.Recursive
import Data.Functor.Foldable hiding (fold)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Foldable
import qualified Data.Set as S
import Text.Casing
import Lens.Micro.Platform
import qualified Data.Map as M

data NumberPreference =
    SmartFloats
  | SmartDoubles
  | FloatNumbers
  | DoubleNumbers
  | ScientificNumbers
  deriving (Show, Eq)

data TextType =
    UseString
  | UseText
  | UseByteString
  deriving (Show, Eq)

data MapType =
    UseMap
  | UseHashMap
  deriving (Show, Eq)

data ListType =
    UseList
  | UseVector
  deriving (Show, Eq)




data Options = Options
  { _tabStop :: Int
  , _numberPreference :: NumberPreference
  , _textType :: TextType
  , _mapType :: MapType
  , _listType :: ListType
  , _includeImports :: Bool
  , _stronglyNormalize :: Bool
  , _strictData :: Bool
  }


data Env = Env
    { _options :: Options
    , _indentationLevel  :: Int
    }

makeLenses ''Options
makeLenses ''Env

defaultOptions :: Options
defaultOptions = Options
    { _tabStop = 2
    , _numberPreference = DoubleNumbers
    , _textType = UseText
    , _mapType = UseMap
    , _listType = UseList
    , _includeImports = False
    , _stronglyNormalize = True
    , _strictData = False
    }

performantOptions :: Options
performantOptions = Options
    { _tabStop = 2
    , _numberPreference = DoubleNumbers
    , _textType = UseText
    , _mapType = UseMap
    , _listType = UseList
    , _includeImports = False
    , _stronglyNormalize = True
    -- TODO
    , _strictData = True
    }


data NumberType = Fractional | Whole
  deriving (Show, Eq, Ord)

type StructName = T.Text
data RecordType = Ref | Structure
data Struct (r :: RecordType) where
        SArray :: Struct r -> Struct r
        SRecord :: (HM.HashMap T.Text (Struct 'Structure)) -> Struct 'Structure
        SRecordRef :: StructName -> Struct 'Ref
        SMap :: Struct r -> Struct r
        SBool :: Struct r
        SNumber :: NumberType -> Struct r
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
      NumberF n ->
          SNumber $ if (ceiling n == (floor n :: Int)) then Whole
                                           else Fractional
      BoolF _ -> SBool
      NullF -> SNull

type Normalizer a = Writer (HM.HashMap T.Text (S.Set (HM.HashMap T.Text (Struct 'Ref)))) a

json2Haskell :: Options -> Value -> T.Text
json2Haskell opts v = do
    let struct = analyze v
        allStructs = execWriter $ normalize (nameRecord "model") struct
     in buildAllStructs opts allStructs

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
  SNumber t -> pure $ SNumber t
  SNull -> pure SNull
  SString -> pure SString
  SValue -> pure SValue

parens :: MonadWriter T.Text m => m a -> m a
parens m =
    tell "(" *> m <* tell ")"

line :: (MonadReader Env m, MonadWriter T.Text m) => m a -> m a
line m = do
    n <- view indentationLevel
    tell $ T.replicate n " "
    a <- m
    newline
    return  a

newline :: MonadWriter T.Text m => m ()
newline = tell "\n"

indented :: (MonadReader Env m, MonadWriter T.Text m) => m a -> m a
indented m = do
    n <- view (options . tabStop)
    local (indentationLevel +~ n) m

type Builder a = ReaderT Env (Writer T.Text) ()

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
    SString -> do
        getTextType >>= tell
    SNumber t -> do
        pref <- view (options . numberPreference)
        case (pref, t) of
            (FloatNumbers, _) -> tell "Float"
            (DoubleNumbers, _) -> tell "Double"
            (ScientificNumbers, _) -> tell "Scientific"
            (SmartFloats, Fractional) -> tell "Float"
            (SmartFloats, Whole) -> tell "Int"
            (SmartDoubles, Fractional) -> tell "Double"
            (SmartDoubles, Whole) -> tell "Int"

    SBool -> tell "Bool"
    SValue -> tell "Value"
    SMap s -> do
        txtType <- getTextType
        tell ("Map " <> txtType <> " ") >> parens (buildType s)
    SArray s -> tell "Vector " >> parens (buildType s)
    SRecordRef n -> tell n
  where
    getTextType = do
        view (options . textType) <&> \case
          UseString -> "String"
          UseByteString -> "ByteString"
          UseText -> "Text"
    -- SOptional s -> tell "Maybe " >> parens (builder s)

-- Normalize records to ensure only one name for each (structural) record as well no duplicate
-- names
-- normalizeStructRefs :: HM.HashMap T.Text (S.Set (RecordRep 'Ref)) -> HM.HashMap T.Text (RecordRep 'Ref)
-- normalizeStructRefs hm = _
    -- let listOfAll = M.fromList . expand . HM.toList $ hm
    --  in _
    --   where
    --     expand xs = flip evalStateT mempty $ do
    --         (name, records) <- lift $ xs
    --         record <- lift $ S.toList records
    --         return [(name, record)]

buildAllStructs :: Options -> HM.HashMap T.Text (S.Set (RecordRep 'Ref)) -> T.Text
buildAllStructs opts hm = execWriter . flip runReaderT (Env opts 0) $ do
    flip HM.traverseWithKey hm $ \k v -> do
        buildRecordDef k (head . S.toList $ v)
        newline
