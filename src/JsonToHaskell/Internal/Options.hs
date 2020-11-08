{-# LANGUAGE TemplateHaskell #-}
module JsonToHaskell.Internal.Options where

import Lens.Micro.Platform (makeLenses)

-- | Choose which type to use for Numbers
data NumberType =
    -- | Use 'Int' for whole numbers, 'Float' for decimals
    UseSmartFloats
    -- | Use 'Int' for whole numbers, 'Double' for decimals
  | UseSmartDoubles
    -- | Use 'Float' for all numbers
  | UseFloatNumbers
    -- | Use 'Double' for all numbers
  | UseDoubleNumbers
    -- | Use 'Scientific' for all numbers
  | UseScientificNumbers
  deriving (Show, Eq)

-- | Choose which type to use for strings
data TextType =
    -- | Use 'String' for strings
    UseString
    -- | Use 'Text' for string
  | UseText
    -- | Use 'ByteString' for strings
  | UseByteString
  deriving (Show, Eq)

-- | Choose which type to use for key-value maps
data MapType =
    -- | Use Data.Map
    UseMap
    -- | Use Data.HashMap
  | UseHashMap
  deriving (Show, Eq)

-- | Choose which type to use for arrays
data ListType =
    -- | Use lists
    UseList
    -- | Use vectors
  | UseVector
  deriving (Show, Eq)

-- | Options for module generation
data Options = Options
  { _tabStop :: Int
  , _numberType :: NumberType
  , _textType :: TextType
  , _mapType :: MapType
  , _listType :: ListType
  , _includeImports :: Bool
  -- , _stronglyNormalize :: Bool
  , _strictData :: Bool
  }


makeLenses ''Options

-- | Simple module generation options.
-- These are reasonable defaults for a simple module
simpleOptions :: Options
simpleOptions = Options
    { _tabStop = 2
    , _numberType = UseDoubleNumbers
    , _textType = UseText
    , _mapType = UseMap
    , _listType = UseList
    , _includeImports = False
    -- , _stronglyNormalize = True
    , _strictData = False
    }

-- | Use more performant data types, use these for production apps.
performantOptions :: Options
performantOptions = Options
    { _tabStop = 2
    , _numberType = UseDoubleNumbers
    , _textType = UseText
    , _mapType = UseMap
    , _listType = UseList
    , _includeImports = False
    -- TODO
    -- , _stronglyNormalize = True
    , _strictData = True
    }


