{-# LANGUAGE TemplateHaskell #-}
module JsonToHaskell.Internal.Options where

import Lens.Micro.Platform (makeLenses)

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

