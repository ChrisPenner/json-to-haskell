{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Flags where

import Options.Applicative
import JsonToHaskell
import Text.RawString.QQ (r)

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionParser <**> helper) fullDesc

parseNumberType :: ReadM NumberType
parseNumberType = maybeReader $ \case
  "smart-floats" -> Just UseSmartFloats
  "smart-doubles" -> Just UseSmartDoubles
  "floats" -> Just UseFloats
  "doubles" -> Just UseDoubles
  _ -> Nothing

parseTextType :: ReadM TextType
parseTextType = maybeReader $ \case
  "string" -> Just UseString
  "text" -> Just UseText
  "bytestring" -> Just UseByteString
  _ -> Nothing

parseMapType :: ReadM MapType
parseMapType = maybeReader $ \case
  "map" -> Just UseMap
  "hashmap" -> Just UseHashMap
  _ -> Nothing

parseListType :: ReadM ListType
parseListType = maybeReader $ \case
  "list" -> Just UseList
  "vector" -> Just UseVector
  _ -> Nothing



optionParser :: Parser Options
optionParser = do
    _tabStop <- option auto
                (short 't'
                 <> long "tab-stop"
                 <> help "Number of spaces to indent each level."
                 <> value 2)
    _numberType <- option parseNumberType
                (short 'n'
                 <> long "numbers"
                 <> value UseSmartDoubles
                 <> help [r|Type to use for numbers.
Options:
'smart-floats':
  Use floats for numbers with decimals, Int for whole numbers.
'smart-doubles':
  Use floats for numbers with decimals, Int for whole numbers.
'floats':
  Use floats for all numbers.
'doubles':
  Use doubles for all numbers.
'scientific':
  Use scientific for all numbers.
|])
    _textType <- option parseTextType
                (short 's'
                 <> long "strings"
                 <> value UseText
                 <> help [r|Type to use for strings.
Options:
'string':
  Use String for strings.
'text':
  Use Text for strings.
'bytestring':
  Use ByteString for strings.
|])
    _mapType <- option parseMapType
                (short 'm'
                 <> long "maps"
                 <> value UseMap
                 <> help [r|Type to use for maps.
Options:
'map':
  Use Data.Map for maps.
'hashmap':
  Use Data.HashMap for maps.
|])
    _listType <- option parseListType
                (short 'l'
                 <> long "lists"
                 <> value UseList
                 <> help [r|Type to use for lists.
Options:
'list':
  Use [] for lists.
'vector':
  Use Data.Vector for lists.
|])
    _includeHeader <- flag True False
                (long "no-module-header"
                 <> help [r|Omit the module header containing language extensions, module definition and imports.|])
    _strictData <- flag False True
                (long "strict"
                 <> help [r|Use strict record fields.|])
    pure $ Options {.. }
