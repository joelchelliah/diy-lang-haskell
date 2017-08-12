module Test_7_UsingTheLanguage where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment                     (extend, lookup)
import           Evaluator                       (evaluate)
import           Interpreter                     (interpret, interpretFile,
                                                  stdLibEnv)
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
      , ("#t", "(>= 2 2)")
      , ("#t", "(>= 2 1)")
      ]


testingLessOrEqual :: TestTree
testingLessOrEqual = testCase
  "\n Test 7.6 - The `<=` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#t", "(<= 1 2)")
      , ("#t", "(<= 2 2)")
      , ("#f", "(<= 2 1)")
      ]


testingLessThan :: TestTree
testingLessThan = testCase
  "\n Test 7.7 - The `<` function" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("#t", "(< 1 2)")
      , ("#f", "(< 2 2)")
      , ("#f", "(< 2 1)")
      ]


-- Lets also implement some basic list functions.
-- These should be pretty easy with some basic recursion.


testingLength :: TestTree
testingLength = testCase
  "\n Test 7.8 - The `length` function. \n\
  \ Count the number of elements in the list" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("5", "(length '(1 2 3 4 5))"           )
      , ("3", "(length '(#t '(1 2 3) 'foo-bar))")
      , ("0", "(length '())"                    )
      ]


testingSum :: TestTree
testingSum = testCase
  "\n Test 7.9 - The `sum` function. \n\
  \ Calculate the sum of all the elements in the list" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("5" , "(sum '(1 1 1 1 1))")
      , ("10", "(sum '(1 2 3 4))"  )
      , ("0" , "(sum '())"         )
      ]


testingRange :: TestTree
testingRange = testCase
  "\n Test 7.10 - The `range` function. \n\
  \ Given two arguments defining the (inclusive) bounds of \n\
  \ a range, create a list with all the numbers within that range" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("(1 2 3 4 5)", "(range 1 5)")
      , ("(1)"        , "(range 1 1)")
      , ("()"         , "(range 2 1)")
      ]


testingAppend :: TestTree
testingAppend = testCase
  "\n Test 7.11 - The `append` function. \n\
  \ The `append` function should merge two lists together \n\
  \ by placing the second list right behind the first one" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("()"            , "(append '() '())"           )
      , ("(1)"           , "(append '() '(1))"          )
      , ("(2)"           , "(append '(2) '())"          )
      , ("(1 2 3 4 5)"   , "(append '(1 2) '(3 4 5))"   )
      , ("(#t #f 'maybe)", "(append '(#t) '(#f 'maybe))")
      ]


testingReverse :: TestTree
testingReverse = testCase
  "\n Test 7.12 - The `reverse` function. \n\
  \ The `reverse` function should reverse the order of the list. \n\
  \ Tip: See if you can make use of the last function you just added" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("()"       , "(reverse '())"       )
      , ("(1)"      , "(reverse '(1))"      )
      , ("(4 3 2 1)", "(reverse '(1 2 3 4))")
      ]


-- Our standard library should contain these three fundamental functions:
-- `filter`, `map` and `fold`.


testingFilter :: TestTree
testingFilter = testCase
  "\n Test 7.13 - The `filter` function. \n\
  \ The `filter` function should remove all elements that do \n\
  \ not satisfy the given predicate, from the list" $ do

    env <- stdLibEnv

    let evenFn    = "\n\
        \(define even\n\
        \    (lambda (x)\n\
        \        (eq (mod x 2) 0)))"
        (_, env') = interpret evenFn env
        input     = "(filter even '(1 2 3 4 5 6))"
        expected  = "(2 4 6)"

    assertInterpretWithEnvironment env' (expected, input)


testingMap :: TestTree
testingMap = testCase
  "\n Test 7.14 - The `map` function. \n\
  \ The `map` function should apply the given function \n\
  \ to all the elements of the list" $ do

    env <- stdLibEnv

    let incFn     = "\n\
        \(define inc\n\
        \    (lambda (x) (+ 1 x)))"
        (_, env') = interpret incFn env
        input     = "(map inc '(1 2 3))"
        expected  = "(2 3 4)"

    assertInterpretWithEnvironment env' (expected, input)


testingFold :: TestTree
testingFold = testCase
  "\n Test 7.15 - The `fold` function. \n\
  \ Given a function, a default value and a list as arguments, \n\
  \ `fold` should produce its result, by recursively combining each \n\
  \ element of the list with the already accumulated result, using the \n\
  \ given function. \n\
  \ Tip: have a look at: \n\
  \ http://en.wikipedia.org/wiki/Fold_(higher-order_function)" $ do

    env <- stdLibEnv

    let maxFn     = "\n\
        \(define max\n\
        \    (lambda (a b)\n\
        \        (if (> a b) a b)))"
        (_, env') = interpret maxFn env
        input     = "(fold max 0 '(1 6 3 2))"
        expected  = "6"

    -- Evaluates as (max 1 (max 6 (max 3 (max 2 0)))) -> 6
    assertInterpretWithEnvironment env' (expected, input)


testingSumThroughFold :: TestTree
testingSumThroughFold = testCase
  "\n Test 7.16 - Summing with the `fold` function. \n\
  \ We should be able to find the sum of a list by `fold`-ing over it" $ do

    env <- stdLibEnv

    let addFn     = "\n\
        \(define add\n\
        \    (lambda (a b) (+ a b)))"
        (_, env') = interpret addFn env
        input     = "(fold add 0 (range 1 4))"
        expected  = "10"

    -- Can we use this to simplify our `sum` function?
    -- Are there any other functions that can also be written using `fold` ?
    assertInterpretWithEnvironment env' (expected, input)


-- Finally, no stdlib is complete without a sorting algorithm.
-- Quicksort or mergesort might be good options, but you choose which
-- ever one you prefer.
-- You might want to implement a few helper functions for this one.


testingSort :: TestTree
testingSort = testCase
  "\n Test 7.17 - The `sort` function. \n\
  \ It should sort the given list of numbers so that they are \n\
  \ in ascending order" $ do

    env <- stdLibEnv

    mapM_ (assertInterpretWithEnvironment env)
      [ ("()"             , "(sort '())"             )
      , ("(1)"            , "(sort '(1))"            )
      , ("(1 2 3 4 5 6 7)", "(sort '(6 3 7 2 4 1 5))")
      , ("(1 2 3 4 5 6 7)", "(sort '(1 2 3 4 5 6 7))")
      , ("(1 2 3 4 5 6 7)", "(sort '(7 6 5 4 3 2 1))")
      , ("(1 1 1)"        , "(sort '(1 1 1))"        )
      ]


usingTheLanguageTests :: TestTree
usingTheLanguageTests =
  testGroup "- Using the language -"
    [ testingNot
    , testingOr
    , testingAnd
    , testingXor
    , testingGreaterOrEqual
    , testingLessOrEqual
    , testingLessThan
    , testingLength
    , testingSum
    , testingRange
    , testingAppend
    , testingReverse
    , testingFilter
    , testingMap
    , testingFold
    , testingSumThroughFold
    , testingSort
    ]
