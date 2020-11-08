{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module JsonToHaskell.Internal.Parser where

import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Aeson.Extra.Recursive (ValueF(..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Functor.Foldable (cataA)
import Data.Foldable (for_)
import Data.Either (fromRight)
import Text.Casing (toPascal, fromAny)
import qualified Data.List as L
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S

-- | Used to track whether the value was fractional or whole.
data NumberVariant = Fractional | Whole
  deriving (Show, Eq, Ord)

-- a DataKind for tracking whether a structure contains nested structs or Record Names
data RecordType = Ref | Structure
-- | The representation of a record's field types
type RecordFields r = HM.HashMap T.Text (Struct r)
-- | The recursive representation of the "type" of a JSON value
data Struct (r :: RecordType) where
        SArray :: Struct r -> Struct r
        SRecord :: (RecordFields 'Structure) -> Struct 'Structure
        SRecordRef :: T.Text -> Struct 'Ref
        SMap :: Struct r -> Struct r
        SBool :: Struct r
        SNumber :: NumberVariant -> Struct r
        SNull :: Struct r
        SString :: Struct r
        SValue :: Struct r
deriving instance Show (Struct r)
deriving instance Eq (Struct r)
deriving instance Ord (Struct r)

type AnalyzeM a =
    ReaderT T.Text
            (State (M.Map (RecordFields 'Structure)
                          (NES.NESet T.Text)))
            a

-- | Convert a 'Value' into a Typed representation of its structure, tracking reasonable names
-- for each subrecord along the way
analyze :: Value
        -> M.Map (RecordFields 'Structure) (NES.NESet T.Text)
analyze value =
    flip execState mempty . flip runReaderT "Model" $ cataA alg value
  where
    -- Algebra for reducing a JSON ValueF from the bottom up.
    alg :: ValueF (AnalyzeM (Struct 'Structure))
        -> AnalyzeM (Struct 'Structure)
    alg = \case
        ObjectF m     -> do
            m' <- flip HM.traverseWithKey m
                $ \fieldName substructM -> do
                    -- Pass down the current field name as a heuristic for picking a
                    -- reasonable name for records encountered at the lower levels
                    local (const fieldName) substructM
            nameRecord m'
            pure $ SRecord m'
        ArrayF itemsM -> do
            items <- sequenceA itemsM
            pure $ case (items V.!? 0) of
                Just s  -> SArray s
                Nothing -> SArray SValue
        StringF _     -> pure SString
        NumberF n     -> pure . SNumber
            $ if (ceiling n == (floor n :: Int))
                then Whole
                else Fractional
        BoolF _       -> pure SBool
        NullF         -> pure SNull

    -- Pair the given record with the name in scope
    nameRecord :: RecordFields 'Structure ->  AnalyzeM ()
    nameRecord record = do
        name <- asks toRecordName
        modify . flip M.alter record $ \case
          Nothing -> Just $ NES.singleton name
          Just s -> Just $ NES.insert name s

-- | Given a mapping of structures to name candidates, pick names for each record, avoiding
-- duplicates
canonicalizeRecordNames :: M.Map (RecordFields 'Structure) (NES.NESet T.Text) -> BM.Bimap T.Text (RecordFields 'Structure)
canonicalizeRecordNames m =
    flip execState BM.empty $ do
        -- Pick names for those with the fewest candidates first
        -- This helps give everything a "good" name
        for_ (L.sortOn (NES.size . snd) . M.toList $ m) $ \(struct, names) -> do
            existingNames <- get
            let bestName = chooseBestName names (S.fromAscList . BM.keys $ existingNames)
            modify (BM.insert bestName struct)

-- | Choose a "fresh" name given a list of candidates and a map of names which have already
-- been chosen.
chooseBestName :: NES.NESet T.Text -> S.Set T.Text -> T.Text
chooseBestName candidates takenNames =
    case S.lookupMin $ S.difference (NES.toSet candidates) takenNames of
        Nothing -> makeUnique (NES.findMin candidates) takenNames
        Just name -> name

-- | Given a name candidate, make it unique amongs the set of taken names by appending
-- the lowest number which isn't yet taken. E.g. if "name" is taken, try "name2", "name3"
-- ad infinitum
makeUnique :: T.Text -> S.Set T.Text -> T.Text
makeUnique candidate takenNames =
    -- construct an infinite candidates list of ["name", "name2", "name3", ...]
    let candidates = (candidate <>) <$> ("" : fmap (T.pack . show) [(2 :: Int)..])
    -- Get the first unique name from the list.
    -- The list is infinite, so head is safe here.
     in head . filter (not . flip S.member takenNames) $ candidates

-- | Switch literal struct definitions with their "names"
addReferences :: BM.Bimap T.Text (RecordFields 'Structure) -> Struct 'Structure -> Struct 'Ref
addReferences m =
  \case
    SNull -> SNull
    SString -> SString
    SNumber t -> SNumber t
    SBool -> SBool
    SValue -> SValue
    SMap s -> SMap (addReferences m s)
    SArray s -> SArray (addReferences m s)
    SRecord s -> SRecordRef . fromRight (error "Expected record name but wasn't found") $ BM.lookupR s m

-- | Clean a name into a valid Haskell record name
toRecordName :: T.Text -> T.Text
toRecordName = T.filter (isAlphaNum) . T.pack . toPascal . fromAny . T.unpack . T.dropWhile (not . isAlpha)
