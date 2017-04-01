module Test_7_UsingTheLanguage where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment                     (extend, lookup)
import           Evaluator                       (evaluate)
import           Interpreter                     (interpretFile)
import           Parser                          (parse)
import           Prelude                         hiding (lookup)
import           TestHelper
import           Types

--
-- Consider these tests as suggestions for what a standard library for
-- your language could contain. Each test will check the implementation
-- of one stdlib function.
--
-- Place your functions in the `stdlib.diy` file in the root folder. The first
-- function, `not` is already defined for you. It's your job to create
-- the rest... or perhaps somthing completely different?
--
-- Anything you put in `stdlib.diy` is also available from the REPL,
-- so feel free to test your functions out there as well:
--
--      $ ./repl
--      →  (not #t)
--      #f
--
-- Note that in these tests, `interpret` is used. In addition to parsing
-- and evaluating the expression, it also unparses the result before
-- returning it (i.e. return "#t" instead of <DiyBool True>).
--

testingNot :: TestTree
testingNot = testCase
  "\n Test 7.1 - The `not` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#t", "(not #f)")
      , ("#f", "(not #t)")
      ]


testingOr :: TestTree
testingOr = testCase
  "\n Test 7.2 - The `or` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#f", "(or #f #f)")
      , ("#t", "(or #t #f)")
      , ("#t", "(or #f #t)")
      , ("#t", "(or #t #t)")
      ]


testingAnd :: TestTree
testingAnd = testCase
  "\n Test 7.3 - The `and` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#f", "(and #f #f)")
      , ("#f", "(and #t #f)")
      , ("#f", "(and #f #t)")
      , ("#t", "(and #t #t)")
      ]


testingXor :: TestTree
testingXor = testCase
  "\n Test 7.4 - The `xor` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#f", "(xor #f #f)")
      , ("#t", "(xor #t #f)")
      , ("#t", "(xor #f #t)")
      , ("#f", "(xor #t #t)")
      ]


-- The language core just contains the > operator.
-- It's time to implement the rest!


testingGreaterOrEqual :: TestTree
testingGreaterOrEqual = testCase
  "\n Test 7.5 - The `>=` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#f", "(>= 1 2)")
      , ("#t", "(>= 1 2)")
      , ("#t", "(>= 2 1)")
      ]



usingTheLanguageTests :: TestTree
usingTheLanguageTests =
  testGroup "- Using the language -"
    [ testingNot
    , testingOr
    , testingAnd
    , testingXor
    , testingGreaterOrEqual
    ]
