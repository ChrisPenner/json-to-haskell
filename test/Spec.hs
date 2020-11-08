{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import JsonToHaskell
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import Text.RawString.QQ (r)

shouldRender :: BL.ByteString -> T.Text -> Expectation
shouldRender inp out =
    (jsonToHaskell simpleOptions{_includeHeader=False, _includeInstances=False} (fromMaybe (error $ "bad json: " <> BC.unpack inp) $ decode inp))
      `shouldBe` T.strip out


main :: IO ()
main = hspec spec

-- TODO: Should probably alphabetize records internally to ensure consistent order.
spec :: Spec
spec = do
    describe "analyze" $ do
        it "should build a record with all types" $ do
                [r|
{ "a": 1
, "b": "hi"
, "c": [null]
, "d": 2.5
}
                |]
               `shouldRender`
                [r|
data Model = Model
  { modelA :: Double
  , modelD :: Double
  , modelB :: Text
  , modelC :: [Maybe Value]
  } deriving (Show, Eq, Ord)
                |]
        it "should share records definitions for identical subrecords" $ do
                [r|
{ "a": { "name": "bob", "age": 20 }
, "b": { "name": "alice", "age": 22 }
}
                |]
               `shouldRender`
                [r|
data A = A
  { aAge :: Double
  , aName :: Text
  } deriving (Show, Eq, Ord)

data Model = Model
  { modelA :: A
  , modelB :: A
  } deriving (Show, Eq, Ord)
                |]
        it "should pick good names for differing records which share field names" $ do
                [r|
{ "a": { "field": {"name": "bob" } }
, "b": { "field": {"age": 22 } }
}
                |]
               `shouldRender`
                [r|
data A = A
  { aField :: Field
  } deriving (Show, Eq, Ord)

data B = B
  { bField :: Field2
  } deriving (Show, Eq, Ord)

data Field = Field
  { fieldName :: Text
  } deriving (Show, Eq, Ord)

data Field2 = Field2
  { field2Age :: Double
  } deriving (Show, Eq, Ord)

data Model = Model
  { modelA :: A
  , modelB :: B
  } deriving (Show, Eq, Ord)
                |]
        it "should pick the best name if there are multiple possible names but some conflict" $ do
                [r|
{ "a": { "field": {"name": "bob" } }
, "b": { "field": {"age": 22 } }
, "c": { "other": {"age": 22 } }
}
                |]
               `shouldRender`
                [r|
data A = A
  { aField :: Field
  } deriving (Show, Eq, Ord)

data B = B
  { bField :: Other
  } deriving (Show, Eq, Ord)

data C = C
  { cOther :: Other
  } deriving (Show, Eq, Ord)

data Field = Field
  { fieldName :: Text
  } deriving (Show, Eq, Ord)

data Model = Model
  { modelA :: A
  , modelB :: B
  , modelC :: C
  } deriving (Show, Eq, Ord)

data Other = Other
  { otherAge :: Double
  } deriving (Show, Eq, Ord)
                |]

        it "should sanitize weird field and record names" $ do
                [r|
{ "'t\"!h.*#e nam9e": {"'9sub--field": "bob" }
}
                |]
               `shouldRender`
                [r|
data Model = Model
  { modelTheNam9e :: TheNam9e
  } deriving (Show, Eq, Ord)

data TheNam9e = TheNam9e
  { theNam9eSubField :: Text
  } deriving (Show, Eq, Ord)
                |]
