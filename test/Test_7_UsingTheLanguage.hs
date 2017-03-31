module Test_7_UsingTheLanguage where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment                     (extend, lookup)
import           Evaluator                       (evaluate)
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

    -- TODO: Load stdlib into the environment
    let env = Environment []

    mapM_ assertInterpretWithoutEnvironment
      [ ("#t", "(not #f)")
      , ("#f", "(not #t)")
      ]


creatingListWithCons :: TestTree
creatingListWithCons = testCase
  "\n Test 6.2 - Creating a list with `cons`. \n\
  \ The `cons` function should prepend an element to the front \n\
  \ of a list" $ do

    let input    = parse "(cons 0 '(1 2 3))"
        expected = parse "(0 1 2 3)"

    assertEvaluateWithoutEnvironment (expected, input)


creatingLongerListsWithCons :: TestTree
creatingLongerListsWithCons = testCase
  "\n Test 6.3 - `cons` should evaluate it's arguments. \n\
  \ Like all the other special forms and functions in our language, \n\
  \ `cons` is call-by-value. This means that the arguments must be \n\
  \ evaluated before we create the list with their values" $ do

    let input    = parse "(cons 3 (cons (- 4 2) (cons 1 '())))"
        expected = parse "(3 2 1)"

    assertEvaluateWithoutEnvironment (expected, input)


gettingFirstElementFromList :: TestTree
gettingFirstElementFromList = testCase
  "\n Test 6.4 - Getting the first element from a list. \n\
  \ `head` should extract the first element of a list" $

    mapM_ assertEvaluateWithoutEnvironment
      [ (DiyInt 1, parse "(head '(1))")
      , (DiyInt 2, parse "(head '(2 3 4 5))")
      ]


gettingFirstElementFromEmptyList :: TestTree
gettingFirstElementFromEmptyList = testCase
  "\n Test 6.5 - Getting the first element from an empty list. \n\
  \ If the list is empty, there is no first element, so `head` \n\
  \  should produce a <DiyError AccessingEmptyList>" $ do

    let input    = parse "(head (quote ()))"
        expected = DiyError AccessingEmptyList

    assertEvaluateWithoutEnvironment (expected, input)


callingHeadOnSomethingElse :: TestTree
callingHeadOnSomethingElse = testCase
  "\n Test 6.6 - Calling `head` on something else. \n\
  \ It only makes sense to call `head` on a list. Anything else \n\
  \ should produce a <DiyError NotAList>" $ do

    let input    = parse "(head #t)"
        expected = DiyError NotAList

    assertEvaluateWithoutEnvironment (expected, input)


gettingTheTailOfList :: TestTree
gettingTheTailOfList = testCase
  "\n Test 6.7 - Getting the tail of a list. \n\
  \ `tail` should return the remainder of the list after \n\
  \ removing the first element" $

    mapM_ assertEvaluateWithoutEnvironment
      [ (DiyList [DiyInt 2, DiyInt 3], parse "(tail '(1 2 3))")
      , (DiyList [], parse "(tail '(1))")
      ]


gettingTheTailOfEmptyList :: TestTree
gettingTheTailOfEmptyList = testCase
  "\n Test 6.8 - Getting the tail of an empty list. \n\
  \ If the list is empty, there is no tail, so `tail` should \n\
  \ produce a <DiyError AccessingEmptyList>" $ do

    let input    = parse "(tail (quote ()))"
        expected = DiyError AccessingEmptyList

    assertEvaluateWithoutEnvironment (expected, input)


callingTailOnSomethingElse :: TestTree
callingTailOnSomethingElse = testCase
  "\n Test 6.9 - Calling `tail` on something else. \n\
  \ It only makes sense to call `tail` on a list. Anything else \n\
  \ should produce a <DiyError NotAList>" $ do

    let input    = parse "(tail 1)"
        expected = DiyError NotAList

    assertEvaluateWithoutEnvironment (expected, input)


checkingIfListIsEmpty :: TestTree
checkingIfListIsEmpty = testCase
  "\n Test 6.10 - Checking if a list is empty. \n\
  \ The `empty` form checks whether or not a list is empty" $

    mapM_ assertEvaluateWithoutEnvironment
      [ (DiyBool False, parse "(empty '(1 2 3))"   )
      , (DiyBool False, parse "(empty '(1))"       )
      , (DiyBool True , parse "(empty '())"        )
      , (DiyBool True , parse "(empty (tail '(1)))")
      ]


headTailAndEmptyEvaluateTheirArguments :: TestTree
headTailAndEmptyEvaluateTheirArguments = testCase
  "\n Test 6.11 - Function arguments are evaluated first. \n\
  \ Calls to `head`, `tail`, `empty` (or any other special forms) \n\
  \ should always evaluate their arguments first" $ do

    let nullListEnv = Environment [("somelist", DiyList [])]
        someListEnv = Environment [("somelist", DiyList [ DiyInt 1
                                                        , DiyInt 2
                                                        , DiyInt 3
                                                        ]
                                  )]

    assertEvaluateWithEnvironment nullListEnv (DiyBool True , parse "(empty somelist)")

    mapM_ (assertEvaluateWithEnvironment someListEnv)
          [ (DiyInt 1                    , parse "(head somelist)" )
          , (DiyList [DiyInt 2, DiyInt 3], parse "(tail somelist)" )
          , (DiyBool False               , parse "(empty somelist)")
          ]


usingTheLanguageTests :: TestTree
usingTheLanguageTests =
  testGroup "- Using the language -"
    [ testingNot
    , creatingListWithCons
    , creatingLongerListsWithCons
    , gettingFirstElementFromList
    , gettingFirstElementFromEmptyList
    , callingHeadOnSomethingElse
    , gettingTheTailOfList
    , gettingTheTailOfEmptyList
    , callingTailOnSomethingElse
    , checkingIfListIsEmpty
    , headTailAndEmptyEvaluateTheirArguments
    ]
