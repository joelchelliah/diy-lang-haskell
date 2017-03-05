import           Test.Tasty
import           Test.Tasty.Ingredients.FailFast

import           Test_1_Parser

main :: IO ()
main = defaultMainWithIngredients (map failFast defaultIngredients) tests

tests :: TestTree
tests = testGroup "DIY Lang Tests"
  [ parsingTests
  ]
