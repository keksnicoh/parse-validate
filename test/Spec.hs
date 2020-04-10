import Test.Hspec
import qualified ValidateSpec
import qualified ParseSpec

main :: IO ()
main = hspec $ do
  describe "Validate" ValidateSpec.spec
  describe "Parse" ParseSpec.spec