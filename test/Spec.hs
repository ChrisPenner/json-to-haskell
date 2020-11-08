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
  { a :: Double
  , d :: Double
  , b :: Text
  , c :: [Maybe Value]
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
  { age :: Double
  , name :: Text
  } deriving (Show, Eq, Ord)

data Model = Model
  { a :: A
  , b :: A
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
  { field :: Field
  } deriving (Show, Eq, Ord)

data B = B
  { field :: Field2
  } deriving (Show, Eq, Ord)

data Field = Field
  { name :: Text
  } deriving (Show, Eq, Ord)

data Field2 = Field2
  { age :: Double
  } deriving (Show, Eq, Ord)

data Model = Model
  { a :: A
  , b :: B
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
  { field :: Field
  } deriving (Show, Eq, Ord)

data B = B
  { field :: Other
  } deriving (Show, Eq, Ord)

data C = C
  { other :: Other
  } deriving (Show, Eq, Ord)

data Field = Field
  { name :: Text
  } deriving (Show, Eq, Ord)

data Model = Model
  { a :: A
  , b :: B
  , c :: C
  } deriving (Show, Eq, Ord)

data Other = Other
  { age :: Double
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
  { theNam9e :: TheNam9e
  } deriving (Show, Eq, Ord)

data TheNam9e = TheNam9e
  { subField :: Text
  } deriving (Show, Eq, Ord)
                |]
