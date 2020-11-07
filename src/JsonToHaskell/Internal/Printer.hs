{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module JsonToHaskell.Internal.Printer where

import JsonToHaskell.Internal.Parser
import JsonToHaskell.Internal.Options
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (for_, fold)
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Casing (toCamel, fromAny)
import Data.Char (isAlpha, isAlphaNum)
import Lens.Micro.Platform (view, (+~), (<&>))


-- | Convert a name into a valid haskell field name
toFieldName :: T.Text -> T.Text
toFieldName = T.filter (isAlphaNum) . T.pack . toCamel . fromAny . T.unpack . T.dropWhile (not . isAlpha)

type StructName = T.Text

-- | Wrap a writer in parens
parens :: MonadWriter T.Text m => m a -> m a
parens m =
    tell "(" *> m <* tell ")"

-- | Embed the given writer at the correct level of indentation and add a newline
line :: (MonadReader Env m, MonadWriter T.Text m) => m a -> m a
line m = do
    n <- view indentationLevel
    tell $ T.replicate n " "
    a <- m
    newline
    return  a

-- | Add a newline
newline :: MonadWriter T.Text m => m ()
newline = tell "\n"

-- | Indent all 'line's of the given writer by one tabstop
indented :: (MonadReader Env m, MonadWriter T.Text m) => m a -> m a
indented m = do
    n <- view (options . tabStop)
    local (indentationLevel +~ n) m

type Builder a = ReaderT Env (Writer T.Text) ()

-- | Write out the Haskell code for a record data type
writeRecord :: StructName -> RecordFields 'Ref -> Builder ()
writeRecord name struct = do
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

-- | Write out the Haskell code for a ToJSON instance for the given record
writeToJSONInstance :: StructName -> RecordFields 'Ref -> Builder ()
writeToJSONInstance name struct = do
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

-- | Write out the Haskell code for a FromJSON instance for the given record
writeFromJSONInstance :: StructName -> RecordFields 'Ref -> Builder ()
writeFromJSONInstance name struct = do
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


-- | Write out the Haskell representation for a given JSON type
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

-- | Write out all the given records and their instances
writeModel :: Options -> BM.Bimap T.Text (RecordFields 'Ref) -> T.Text
writeModel opts (BM.toMap -> m) = execWriter . flip runReaderT (Env opts 0) $ do
    tell . T.unlines $
        [ "{-# LANGUAGE DuplicateRecordFields #-}"
        , "{-# LANGUAGE RecordWildCards #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module Model where"
        , ""
        , "import Prelude (Double, Bool, Show, Eq, Ord, ($), pure)"
        , "import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), object)"
        , "import Data.Aeson.Types (prependFailure, typeMismatch)"
        , "import Data.Text (Text)"
        , "import Data.Vector (Vector)"
        ]

    newline
    void . flip M.traverseWithKey m $ \k v -> do
        writeRecord k v
        newline
    void . flip M.traverseWithKey m $ \k v -> do
        writeToJSONInstance k v
        newline
    void . flip M.traverseWithKey m $ \k v -> do
        writeFromJSONInstance k v
        newline

escapeQuotes :: T.Text -> T.Text
escapeQuotes = T.replace "\"" "\\\""
