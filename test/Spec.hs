import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "analyze" $ do
        it "should build a record" $ do
            True `shouldBe` True
