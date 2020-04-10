module ValidateSpec where
  
import Test.Hspec
import Validate

-- @todo
--    check semigroup, monoid, cofunctor laws using quickcheck
spec :: Spec
spec = do
  describe "validate" $ do
    it "should return the input if the underlying validate accepts it" $
      let innerValidate = Validator $ const Valid
      in validate innerValidate () `shouldBe` Right ()
    it "should return the validation error if the input was invalidated" $
      let innerValidate = Validator $ const (Invalid "foo")
      in validate innerValidate () `shouldBe` Left "foo"
        
  describe "fromPredicate" $ do
    it "should invalidate if predicate returns False" $
      runValidator (fromPredicate (const False)) 5 `shouldBe` Invalid "validate.predicate"
    it "should validate if predicate returns True" $
      runValidator (fromPredicate (const True)) 5 `shouldBe` Valid

  describe "minLength" $ do
    let validate = runValidator (minLength 5)
    it "should not accept 4 >= 5" $ validate "abcd"   `shouldBe` Invalid "validate.length.min"
    it "should accept 5 >= 5"     $ validate "abcde"  `shouldBe` Valid
    it "should accept 6 >= 5"     $ validate "abcdef" `shouldBe` Valid
  
  describe "maxLength" $ do
    let validate = runValidator (maxLength 4) 
    it "should not accept 5 <= 4" $ validate "abcde" `shouldBe` Invalid "validate.length.max"
    it "should accept 4 <= 4"     $ validate "abcd"  `shouldBe` Valid
    it "should accept 3 <= 4"     $ validate "abc"   `shouldBe` Valid

  describe "ordMin" $ do
    let validate = runValidator (ordMin 4)
    it "should not accept 3 >= 4" $ validate 3 `shouldBe` Invalid "validate.min"
    it "should accept 4 >= 4"     $ validate 4 `shouldBe` Valid
    it "should accept 5 >= 4"     $ validate 5 `shouldBe` Valid

  describe "ordMax" $ do
    let validate = runValidator (ordMax 4)
    it "should not accept 5 <= 4" $ validate 5 `shouldBe` Invalid "validate.max"
    it "should accept 4 <= 4"     $ validate 4 `shouldBe` Valid
    it "should accept 3 <= 4"     $ validate 3 `shouldBe` Valid

  describe "label" $
    it "should override the validation message" $
      let validate = Validator $ const (Invalid "foo")
      in runValidator (validate `label` "derk") 5 `shouldBe` Invalid "derk"