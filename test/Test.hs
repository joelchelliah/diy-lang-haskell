import           Test.Tasty
import           Test.Tasty.Ingredients.FailFast

import           Test_1_Parser
import           Test_2_EvaluatingSimpleExpressions
import           Test_3_EvaluatingComplexExpressions
import           Test_4_WorkingWithVariables
import           Test_5_AddingFunctionsToTheMix

main :: IO ()
-- main = defaultMainWithIngredients (map failFast defaultIngredients) tests
main = defaultMain tests

tests :: TestTree
tests = testGroup "DIY Lang Tests"
  [ parsingTests
  , evaluatingSimpleExpressionsTests
  , evaluatingComplexExpressionsTests
  , workingWithVariabesTests
  , addingFunctionsToTheMixTests
  ]
