module Test_2_EvaluatingSimpleExpressions where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Evaluator
import           Parser                          (ParseErrorType (..),
                                                  Parsed (..), unparse)


-- We will start by implementing evaluation of simple expressions.

evaluateBoolean :: TestTree
evaluateBoolean = testCase
  "\n Test 2.1 - Evaluating a single boolean. \n\
  \ Booleans should evaluate to themselves" $ do

    let input    = "foo"
        env      = ""
        eval ast = evaluate ast env

    assertEqual (desc input env) (ParsedBool True) $ eval (ParsedBool True)
    assertEqual (desc input env) (ParsedBool False) $ eval (ParsedBool False)


desc :: String -> String -> String
desc input env = "evaluate \"" ++ input ++ "\"" ++ env ++ "\""

evaluatingSimpleExpressionsTests :: TestTree
evaluatingSimpleExpressionsTests =
  testGroup "- Evaluating simple expressions -"
    [ evaluateBoolean
    ]
