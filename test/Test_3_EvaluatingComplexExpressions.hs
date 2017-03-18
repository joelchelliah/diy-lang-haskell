module Test_3_EvaluatingComplexExpressions where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment
import           Evaluator
import           Parser                          (parse)
import           TestHelper
import           Types


evaluateNestedExpression :: TestTree
evaluateNestedExpression = testCase
  "\n Test 3.1 - Evaluating a nested expression. \n\
  \ Note that functions should evaluate their arguments \n\
  \ (except `quote`, `if`, etc., which aren't really \n\
  \ functions). Thus, nested expressions should work just fine \n\
  \ without any further work at this point. \n\
  \ If this test is failing, make sure that `+`, `>`, and so \n\
  \ on are evaluating their arguments before operating on them"  $ do

    let input    = parse "(eq #f (> (- (+ 1 3) (* 2 (mod 7 4))) 4))"
        expected = DiyBool True

    assertEvaluateWithoutEnvironment (expected, input)


evaluateBasicIfStatement :: TestTree
evaluateBasicIfStatement = testCase
  "\n Test 3.2 - Evaluating an if statement. \n\
  \ If statements are the basic control structures. \n\
  \ The `if` should first evaluate its first argument. If this \n\
  \ evaluates to <DiyBool True>, then the second argument is \n\
  \ evaluated and returned. Otherwise the third and last argument \n\
  \ is evaluated and returned instead" $ do

    mapM_ assertEvaluateWithoutEnvironment
          [ ( DiyInt 42   , parse "(if #t 42 1000)")
          , ( DiyInt 1000 , parse "(if #f 42 1000)")
          , ( DiyBool True, parse "(if #t #t #f)"  )
          ]


evaluateOnlyTheCorrectBranch :: TestTree
evaluateOnlyTheCorrectBranch = testCase
  "\n Test 3.3 - Only evaluate the correct branch. \n\
  \ The branch of the if statement that is discarded \n\
  \ should never be evaluated" $ do

    let input    = parse "(if #f (this should not be evaluated) 42)"
        expected = DiyInt 42

    assertEvaluateWithoutEnvironment (expected, input)


evaluateIfWithSubExpressions :: TestTree
evaluateIfWithSubExpressions = testCase
  "\n Test 3.4 - Evaluating `if` with sub expressions. \n\
  \ A final test with a more complex if statement. \n\
  \ This test should already be passing if the above ones are" $ do

    let expected = DiyInt 42
        input    = parse "\n\
    \ (if (> 1 2) \n\
    \     (- 1000 1) \n\
    \     (+ 40 (- 3 1))) \n\
    \"

    assertEvaluateWithoutEnvironment (expected, input)


quoteShouldNotEvaluateItsArguments :: TestTree
quoteShouldNotEvaluateItsArguments = testCase
  "\n Test 3.5 - Quote shoud not evaluate its arguments. \n\
  \ Calling `quote`, should still return its argument \n\
  \ without evaluating it. This test should already be passing, \n\
  \ but lets just make sure that `quote` still works as \n\
  \ intended now that we have a few more powerful features." $ do

    let input    = parse "\n\
    \ '(if (> 1 50) \n\
    \      (- 1000 1) \n\
    \      #f)"
        expected = DiyList [ DiySymbol "if"
                           , DiyList [ DiySymbol ">"
                                     , DiyInt 1
                                     , DiyInt 50
                                     ]
                           , DiyList [ DiySymbol "-"
                                     , DiyInt 1000
                                     , DiyInt 1
                                     ]
                           , DiyBool False
                           ]

    assertEvaluateWithoutEnvironment (expected, input)


evaluatingComplexExpressionsTests :: TestTree
evaluatingComplexExpressionsTests =
  testGroup "- Evaluating complex expressions -"
    [ evaluateNestedExpression
    , evaluateBasicIfStatement
    , evaluateOnlyTheCorrectBranch
    , evaluateIfWithSubExpressions
    , quoteShouldNotEvaluateItsArguments
    ]
