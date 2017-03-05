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

    mapM_ assertEval [ ParsedBool True
                     , ParsedBool False
                     ]


evaluateInteger :: TestTree
evaluateInteger = testCase
  "\n Test 2.2 - Evaluating a single integer. \n\
  \ Integers should evaluate to themselves" $ do

    let assertEval i = assertEvaluateWithoutEnvironment (i, i)

    mapM_ assertEval [ ParsedInt 42
                     , ParsedInt 1337
                     , ParsedInt 0
                     ]


evaluateQuote :: TestTree
evaluateQuote = testCase
  "\n Test 2.3 - Evaluating quotes. \n\
  \ When a call is done to the `quote` form, the argument \n\
  \ should be returned without being evaluated" $ do

    let pairWithQuotedExp exp = (ParsedList [ ParsedSymbol "quote", exp] , exp)

    mapM_ assertEvaluateWithoutEnvironment
        $ pairWithQuotedExp <$>
          [ ParsedSymbol "foo"
          , ParsedList []
          , ParsedList [ ParsedInt 0, ParsedInt 69, ParsedBool False]
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
        [ (parse "(atom #t)"      , ParsedBool True )
        , (parse "(atom #f)"      , ParsedBool True )
        , (parse "(atom 42)"      , ParsedBool True )
        , (parse "(atom 'foo)"    , ParsedBool True )
        , (parse "(atom (1 2 #f))", ParsedBool False)
        ]


evaluateEqFunction :: TestTree
evaluateEqFunction = testCase
  "\n Test 2.5 - Evaluating eq. \n\
  \ The `eq` form is used to check whether two expressions are \n\
  \ the same atom. \n\
  \ Note that lists are never equal, because lists are not atoms" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(eq 1 1)"              , ParsedBool True )
        , (parse "(eq 1 2)"              , ParsedBool False)
        , (parse "(eq 'foo 'foo)"        , ParsedBool True )
        , (parse "(eq 'foo 'bar)"        , ParsedBool False)
        -- Lists are never equal, because lists are not atoms
        , (parse "(eq '(1 2 3) '(1 2 3))", ParsedBool False)
        ]


evaluateBasicMathOperators :: TestTree
evaluateBasicMathOperators = testCase
  "\n Test 2.6 - Evaluating basic math operators. \n\
  \ To be able to do anything useful, we need some basic math operators. \n\
  \ Note that `/` represents integer division, and `mod` is \n\
  \ the modulo operator" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(+ 23 19)", ParsedInt  42   )
        , (parse "(- 2 1)"  , ParsedInt  1    )
        , (parse "(/ 6 2)"  , ParsedInt  3    )
        , (parse "(/ 7 2)"  , ParsedInt  3    )
        , (parse "(mod 7 2)", ParsedInt  1    )
        , (parse "(> 7 2)"  , ParsedBool True )
        , (parse "(> 2 7)"  , ParsedBool False)
        , (parse "(> 4 4)"  , ParsedBool False)
        ]


evaluateMathOperatorsOnOtherInput :: TestTree
evaluateMathOperatorsOnOtherInput = testCase
  "\n Test 2.7 - Evaluating math operators on non-numbers. \n\
  \ The math operators should only allow numbers as arguments" $

    mapM_ assertEvaluateWithoutEnvironment
        [ (parse "(+ 1 'foo)"  , ParseError InvalidArgument)
        , (parse "(- 1 'foo)"  , ParseError InvalidArgument)
        , (parse "(/ 1 'foo)"  , ParseError InvalidArgument)
        , (parse "(mod 1 'foo)", ParseError InvalidArgument)
        ]


assertEvaluateWithoutEnvironment :: (Parsed, Parsed) -> Assertion
assertEvaluateWithoutEnvironment (input, expected) =
  assertEqual description expected result

  where description = desc input environment
        result      = evaluate input environment
        environment = ""

desc :: Parsed -> String -> String
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
