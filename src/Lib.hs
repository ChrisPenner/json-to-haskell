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

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson (Value)
import Data.Aeson.Extra.Recursive (ValueF(..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Foldable (for_, fold)
import Data.Functor.Foldable (cata)
import Data.Maybe (catMaybes)
import Data.Either (fromRight)
import Lens.Micro.Platform (makeLenses, (<&>), (+~), view)
import Text.Casing (toPascal, toCamel, fromAny)
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V

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

type Normalizer a = State (M.Map (HM.HashMap T.Text (Struct 'Structure)) (NES.NESet T.Text)) a

json2Haskell :: Options -> Value -> T.Text
json2Haskell opts v = do
    let struct = analyze v
        allStructs = flip execState mempty $ normalize (nameRecord "model") struct
        namedStructs = nameAllRecords allStructs
        referencedStructs = BM.mapR (fmap (dereference namedStructs)) namedStructs
     in buildAllStructs opts referencedStructs

nameAllRecords :: M.Map (RecordRep 'Structure) (NES.NESet T.Text) -> BM.Bimap T.Text (RecordRep 'Structure)
nameAllRecords m =
    flip execState BM.empty $ do
        for_ (L.sortOn (NES.size . snd) . M.toList $ m) $ \(struct, names) -> do
            existingNames <- get
            let bestName = chooseBestName (NES.toList names) existingNames
            modify (BM.insert bestName struct)

dereference :: BM.Bimap T.Text (RecordRep 'Structure) -> Struct 'Structure -> Struct 'Ref
dereference m =
  \case
    SNull -> SNull
    SString -> SString
    SNumber t -> SNumber t
    SBool -> SBool
    SValue -> SValue
    SMap s -> SMap (dereference m s)
    SArray s -> SArray (dereference m s)
    SRecord s -> SRecordRef . fromRight (error "Expected record name but wasn't found") $ BM.lookupR s m

chooseBestName :: Ord a => NE.NonEmpty T.Text -> BM.Bimap T.Text a -> T.Text
chooseBestName (x NE.:| y : ys) m =
    case BM.lookup x m of
        Nothing -> x
        Just _ -> chooseBestName (y NE.:| ys) m
chooseBestName (x NE.:| []) m =
    head . catMaybes . fmap (go . (x <>)) $ ("" : fmap (T.pack . show) [(1 :: Int)..])
  where
    go k = case BM.lookup k m of
        Nothing -> Just k
        Just _ -> Nothing


nameRecord :: T.Text -> RecordRep 'Structure -> Normalizer ()
nameRecord (toRecordName -> name) record = do
    modify $ \m -> M.alter (Just . maybe (NES.singleton name) (NES.insert name)) record m

toRecordName :: T.Text -> T.Text
toRecordName = T.filter (isAlphaNum) . T.pack . toPascal . fromAny . T.unpack . T.dropWhile (not . isAlpha)

toFieldName :: T.Text -> T.Text
toFieldName = T.filter (isAlphaNum) . T.pack . toCamel . fromAny . T.unpack . T.dropWhile (not . isAlpha)


normalize :: (RecordRep 'Structure -> Normalizer ()) -> Struct 'Structure -> Normalizer (Struct 'Structure)
normalize register = \case
  SRecord m -> do
      m' <- flip HM.traverseWithKey m $ \k v -> do
          normalize (nameRecord k) v
      register $ m'
      return $ SRecord m'
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

    indented $ do
      when (HM.null struct) . line $ tell "{"
      for_ (zip [0 :: Int ..] $ HM.toList struct) $ \(i, (k, v)) -> do
        line $ do
            if (i == 0) then tell "{ "
                        else tell ", "
            tell $ toFieldName k
            tell " :: "
            buildType v
    indented . line $ do
        tell "} "
        tell "deriving (Show, Eq, Ord)"

buildToJSONInstance :: StructName -> HM.HashMap T.Text (Struct 'Ref) -> Builder ()
buildToJSONInstance name struct = do
    line $ tell $ "instance ToJSON " <> name <> " where"
    indented $ do
        line $ do
            tell $ "toJSON " <> name
            when (not . HM.null $ struct) $ tell "{..}"
            tell " = object"
        indented $ do
            when (HM.null struct) . line $ tell "["
            for_ (zip [0 :: Int ..] $ HM.keys struct) $ \(i, k) -> do
                line $ do
                    if (i == 0) then tell "[ "
                                else tell ", "
                    tell $ "\"" <> escapeQuotes k <> "\""
                    tell " .= "
                    tell $ toFieldName k
            line . tell $ "] "

buildFromJSONInstance :: StructName -> HM.HashMap T.Text (Struct 'Ref) -> Builder ()
buildFromJSONInstance name struct = do
    line $ tell $ "instance FromJSON " <> name <> " where"
    indented $ do
        line $ tell $ "parseJSON (Object v) = do"
        indented $ do
            for_ (HM.keys struct) $ \k -> do
                line $ do
                    tell $ toFieldName k
                    tell " <- v .: "
                    tell $ "\"" <> escapeQuotes k <> "\""
            line $ do
                tell $ "pure $ " <> name
                when (not . HM.null $ struct) $ tell "{..}"
        line $ tell $ "parseJSON invalid = do"
        indented $ do
          line . tell $ "prependFailure \"parsing " <> name <> " failed, \""
          indented . line . tell $ "(typeMismatch \"Object\" invalid)"


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

buildAllStructs :: Options -> BM.Bimap T.Text (RecordRep 'Ref) -> T.Text
buildAllStructs opts (BM.toMap -> m) = execWriter . flip runReaderT (Env opts 0) $ do
    tell . T.unlines $
        [ "{-# LANGUAGE DuplicateRecordFields #-}"
        , "{-# LANGUAGE RecordWildCards #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module Model where"
        , ""
        , "import Prelude (Double, Bool, Show, Eq, Ord)"
        , "import Data.Aeson (ToJSON)"
        , "import Data.Text (Text)"
        , "import Data.Vector (Vector)"
        ]
    newline
    void . flip M.traverseWithKey m $ \k v -> do
        buildRecordDef k v
        newline
    void . flip M.traverseWithKey m $ \k v -> do
        buildToJSONInstance k v
        newline
    void . flip M.traverseWithKey m $ \k v -> do
        buildFromJSONInstance k v
        newline

escapeQuotes :: T.Text -> T.Text
escapeQuotes = T.replace "\"" "\\\""
