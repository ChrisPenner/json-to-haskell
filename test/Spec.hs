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
  , c :: [(Maybe Value)]
  } deriving (Show, Eq, Ord)
                |]
