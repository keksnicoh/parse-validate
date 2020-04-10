module ParseSpec where
  
import Test.Hspec
import Parse

spec :: Spec
spec = do
  describe "stringParse" $
    it "should pass the string" $
      runParser stringParse "FOO" `shouldBe` Right "FOO"

  describe "intParse" $ do
    it "should parse valid integer" $ do
      runParser intParse "0" `shouldBe` Right 0
      runParser intParse "-1" `shouldBe` Right (-1)
      runParser intParse "234" `shouldBe` Right 234

    it "should trim" $ do
      runParser intParse " -1" `shouldBe` Right (-1)
      runParser intParse "-1 " `shouldBe` Right (-1)

    it "should not parse invalid values" $ do
      runParser intParse "derp" `shouldBe` Left "parse.expected.integer"
      runParser intParse "0.2" `shouldBe` Left "parse.expected.integer"