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
import Lens.Micro.Platform (view, (+~), (<&>))


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

buildRecordDef :: StructName -> RecordFields 'Ref -> Builder ()
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

buildToJSONInstance :: StructName -> RecordFields 'Ref -> Builder ()
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

buildFromJSONInstance :: StructName -> RecordFields 'Ref -> Builder ()
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

buildAllStructs :: Options -> BM.Bimap T.Text (RecordFields 'Ref) -> T.Text
buildAllStructs opts (BM.toMap -> m) = execWriter . flip runReaderT (Env opts 0) $ do
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
