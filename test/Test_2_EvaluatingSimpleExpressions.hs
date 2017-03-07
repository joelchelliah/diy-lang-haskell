module Test_2_EvaluatingSimpleExpressions where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Evaluator
import           Parser                          (parse)
import           Types

-- We will start by implementing evaluation of simple expressions.


evaluateBoolean :: TestTree
evaluateBoolean = testCase
  "\n Test 2.1 - Evaluating a single boolean. \n\
  \ Booleans should evaluate to themselves" $ do

    let assertEval i = assertEvaluateWithoutEnvironment (i, i)

    mapM_ assertEval [ DiyBool True
                     , DiyBool False
                     ]


evaluateInteger :: TestTree
evaluateInteger = testCase
  "\n Test 2.2 - Evaluating a single integer. \n\
  \ Integers should evaluate to themselves" $ do

    let assertEval i = assertEvaluateWithoutEnvironment (i, i)

    mapM_ assertEval [ DiyInt 42
                     , DiyInt 1337
                     , DiyInt 0
                     ]


evaluateQuote :: TestTree
evaluateQuote = testCase
  "\n Test 2.3 - Evaluating quotes. \n\
  \ When a call is done to the `quote` form, the argument \n\
  \ should be returned without being evaluated" $ do

    let pairWithQuotedExp exp = (DiyList [ DiySymbol "quote", exp] , exp)

    mapM_ assertEvaluateWithoutEnvironment
        $ pairWithQuotedExp <$>
          [ DiySymbol "foo"
          , DiyList []
          , DiyList [ DiyInt 0, DiyInt 69, DiyBool False]
          ]


-- From this point, the ASTs might sometimes be too long or cummbersome to
-- write down explicitly, so we'll use `parse` to make them for us.


evaluateAtomFunction :: TestTree
evaluateAtomFunction = testCase
  "\n Test 2.4 - Evaluating atom. \n\
  \ The `atom` form is used to determine whether an expression is an atom. \n\
  \ Atoms are expressions that are not lists, i.e. integers, booleans \n\
  \ or symbols. Remember that the argument to `atom` must be evaluated \n\
  \ before the check is done" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(atom #t)"      , DiyBool True )
        , (parse "(atom #f)"      , DiyBool True )
        , (parse "(atom 42)"      , DiyBool True )
        , (parse "(atom 'foo)"    , DiyBool True )
        , (parse "(atom (1 2 #f))", DiyBool False)
        ]


evaluateEqFunction :: TestTree
evaluateEqFunction = testCase
  "\n Test 2.5 - Evaluating eq. \n\
  \ The `eq` form is used to check whether two expressions are \n\
  \ the same atom. \n\
  \ Note that lists are never equal, because lists are not atoms" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(eq 1 1)"              , DiyBool True )
        , (parse "(eq 1 2)"              , DiyBool False)
        , (parse "(eq 'foo 'foo)"        , DiyBool True )
        , (parse "(eq 'foo 'bar)"        , DiyBool False)
        -- Lists are never equal, because lists are not atoms
        , (parse "(eq '(1 2 3) '(1 2 3))", DiyBool False)
        ]


evaluateBasicMathOperators :: TestTree
evaluateBasicMathOperators = testCase
  "\n Test 2.6 - Evaluating basic math operators. \n\
  \ To be able to do anything useful, we need some basic math operators. \n\
  \ Note that `/` represents integer division, and `mod` is \n\
  \ the modulo operator" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(+ 23 19)", DiyInt  42   )
        , (parse "(- 2 1)"  , DiyInt  1    )
        , (parse "(/ 6 2)"  , DiyInt  3    )
        , (parse "(/ 7 2)"  , DiyInt  3    )
        , (parse "(mod 7 2)", DiyInt  1    )
        , (parse "(> 7 2)"  , DiyBool True )
        , (parse "(> 2 7)"  , DiyBool False)
        , (parse "(> 4 4)"  , DiyBool False)
        ]


evaluateMathOperatorsOnOtherInput :: TestTree
evaluateMathOperatorsOnOtherInput = testCase
  "\n Test 2.7 - Evaluating math operators on non-numbers. \n\
  \ The math operators should only allow numbers as arguments" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(+ 1 'foo)"  , DiyError InvalidArgument)
        , (parse "(- 1 'foo)"  , DiyError InvalidArgument)
        , (parse "(/ 1 'foo)"  , DiyError InvalidArgument)
        , (parse "(mod 1 'foo)", DiyError InvalidArgument)
        ]


assertEvaluateWithoutEnvironment :: (DiyAST, DiyAST) -> Assertion
assertEvaluateWithoutEnvironment (input, expected) =
  assertEqual description expected result

  where description = desc input environment
        result      = evaluate input environment
        environment = ""

desc :: DiyAST -> String -> String
desc input env =
  "evaluate (" ++ show input ++ ") \"" ++ env ++ "\""

evaluatingSimpleExpressionsTests :: TestTree
evaluatingSimpleExpressionsTests =
  testGroup "- Evaluating simple expressions -"
    [ evaluateBoolean
    , evaluateInteger
    , evaluateQuote
    , evaluateAtomFunction
    , evaluateEqFunction
    , evaluateBasicMathOperators
    , evaluateMathOperatorsOnOtherInput
    ]
