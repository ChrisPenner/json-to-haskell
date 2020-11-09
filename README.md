# json-to-haskell

<!-- toc GFM -->

* [Web interface](#web-interface)
* [What is it?](#what-is-it)
* [Installation](#installation)
* [Usage](#usage)
* [Help Wanted](#help-wanted)

<!-- tocstop -->

## Web interface

[Click here for the web interface](http://json-to-haskell.chrispenner.ca/)
[Web interface source code (Miso)](https://github.com/ChrisPenner/json-to-haskell-web)

## What is it?

In goes JSON, out comes Haskell!

A handy tool for quickly spec'ing out types for a given JSON type.

**Note: This tool isn't perfect, but it should get you _most_ of the way there.**

```sh
$ cat > company.json
{ "company": 
 { "employees": 
    [ {"name": "Jon", "age": 32} 
    , {"name": "Alice", "age": 27} 
    ] 
 , "star_rating": 4.7 
 } 
}
```

```haskell
$ cat company.json | json-to-haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), object)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data Company = Company
  { companyStarRating :: Double
  , companyEmployees :: [Employees]
  } deriving (Show, Eq, Ord)

data Employees = Employees
  { employeesAge :: Int
  , employeesName :: Text
  } deriving (Show, Eq, Ord)

data Model = Model
  { modelCompany :: Company
  } deriving (Show, Eq, Ord)

instance ToJSON Company where
  toJSON Company{..} = object
    [ "star_rating" .= companyStarRating
    , "employees" .= companyEmployees
    ]

instance ToJSON Employees where
  toJSON Employees{..} = object
    [ "age" .= employeesAge
    , "name" .= employeesName
    ]

instance ToJSON Model where
  toJSON Model{..} = object
    [ "company" .= modelCompany
    ]

instance FromJSON Company where
  parseJSON (Object v) = do
    companyStarRating <- v .: "star_rating"
    companyEmployees <- v .: "employees"
    pure $ Company{..}
  parseJSON invalid = do
    prependFailure "parsing Company failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Employees where
  parseJSON (Object v) = do
    employeesAge <- v .: "age"
    employeesName <- v .: "name"
    pure $ Employees{..}
  parseJSON invalid = do
    prependFailure "parsing Employees failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelCompany <- v .: "company"
    pure $ Model{..}
  parseJSON invalid = do
    prependFailure "parsing Model failed, "
      (typeMismatch "Object" invalid)
```

## Installation

Take your pick:

```sh
cabal install haskell-to-json
# OR
stack install haskell-to-json
```

## Usage

There's a web interface [here](http://json-to-haskell.chrispenner.ca/) if you prefer.


Otherwise, install the cli and ask for help; the cli will have the most up to date help message:

```sh
$ json-to-haskell --help
Usage: json-to-haskell [-t|--tab-stop ARG] [-n|--numbers ARG] [-s|--strings ARG]
                       [-m|--maps ARG] [-l|--lists ARG] [--no-module-header]
                       [--no-instances] [--no-prefix-record-fields] [--strict]

Available options:
  -t,--tab-stop ARG        Number of spaces to indent each level.
  -n,--numbers ARG         Type to use for numbers.

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

  -s,--strings ARG         Type to use for strings.

                           'string':
                             Use String for strings.
                           'text':
                             Use Text for strings.
                           'bytestring':
                             Use ByteString for strings.

  -m,--maps ARG            Type to use for maps.

                           'map':
                             Use Data.Map for maps.
                           'hashmap':
                             Use Data.HashMap for maps.

  -l,--lists ARG           Type to use for lists.

                           'list':
                             Use [] for lists.
                           'vector':
                             Use Data.Vector for lists.

  --no-module-header       Omit the module header containing language extensions, module definition and imports.
  --no-instances           Omit the ToJSON and FromJSON instances.
  --no-prefix-record-fields
                           Omit record field prefixes.
  --strict                 Use strict record fields.
  -h,--help                Show this help text
```

## Help Wanted

Want to help out?

I'd love to have a Purescript version of the app! It should be pretty easy to port, you'd just need to adapt the "printer" for the parsed data type.
Check out [Printer.hs](./src/JsonToHaskell/Internal/Printer.hs) .

It'd also be really nice to port the whole project to Purescript, the current miso build is difficult to set up locally.
