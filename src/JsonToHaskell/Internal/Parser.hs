{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module JsonToHaskell.Internal.Parser where

import JsonToHaskell.Internal.Options
import Control.Monad.State
import Data.Aeson (Value)
import Data.Aeson.Extra.Recursive (ValueF(..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Foldable (cata)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Either (fromRight)
import Text.Casing (toPascal, toCamel, fromAny)
import qualified Data.List as L
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V

type StructName = T.Text
data RecordType = Ref | Structure
type RecordFields r = HM.HashMap T.Text (Struct r)
data Struct (r :: RecordType) where
        SArray :: Struct r -> Struct r
        SRecord :: (RecordFields 'Structure) -> Struct 'Structure
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

nameAllRecords :: M.Map (RecordFields 'Structure) (NES.NESet T.Text) -> BM.Bimap T.Text (RecordFields 'Structure)
nameAllRecords m =
    flip execState BM.empty $ do
        for_ (L.sortOn (NES.size . snd) . M.toList $ m) $ \(struct, names) -> do
            existingNames <- get
            let bestName = chooseBestName (NES.toList names) existingNames
            modify (BM.insert bestName struct)

dereference :: BM.Bimap T.Text (RecordFields 'Structure) -> Struct 'Structure -> Struct 'Ref
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


nameRecord :: T.Text -> RecordFields 'Structure -> Normalizer ()
nameRecord (toRecordName -> name) record = do
    modify $ \m -> M.alter (Just . maybe (NES.singleton name) (NES.insert name)) record m

toRecordName :: T.Text -> T.Text
toRecordName = T.filter (isAlphaNum) . T.pack . toPascal . fromAny . T.unpack . T.dropWhile (not . isAlpha)

toFieldName :: T.Text -> T.Text
toFieldName = T.filter (isAlphaNum) . T.pack . toCamel . fromAny . T.unpack . T.dropWhile (not . isAlpha)


normalize :: (RecordFields 'Structure -> Normalizer ()) -> Struct 'Structure -> Normalizer (Struct 'Structure)
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
