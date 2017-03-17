module Test_5_AddingFunctionsToTheMix where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment                     (extend, lookup)
import           Evaluator                       (evaluate)
import           Parser                          (parse)
import           Prelude                         hiding (lookup)
import           Types

--
-- This part is all about defining and using functions.
--
-- We'll start by implementing the `lambda` form,
-- which is used to create function closures.
--

lambdaEvaluatesToClosure :: TestTree
lambdaEvaluatesToClosure = testCase
  "\n Test 5.1 - The lambda form evaluates to a closure. \n\
  \ Closures are represented as <DiyClosure DiyFunction Environment> \n\
  \  in our AST"  $ do

    let input = parse "(lambda, (), 42)"
        env   = Environment []

    assertIsClosure . fst $ evaluate input env


closureKeepsCopyOfEnvironment :: TestTree
closureKeepsCopyOfEnvironment = testCase
  "\n Test 5.2 - A closure copies the environment. \n\
  \ The closure should keep a copy of the environment where it was \n\
  \ defined. Once we start calling functions later, we'll need \n\
  \ access to the environment from when the function was created \n\
  \ in order to resolve all free variables" $ do

    let input        = parse "(lambda, (), 42)"
        env          = Environment [("foo", DiyInt 42)]
        (closure, _) = evaluate input env

    assertEqual "Should be the same environment" env $ localEnv closure


closureHoldsFunction :: TestTree
closureHoldsFunction = testCase
  "\n Test 5.3 - A closure holds a function. \n\
  \ The closure contains a parameter list and a function body. They \n\
  \ are represented as a <DiyFunction [DiyAST] DiyAST> in our AST" $ do

    let input          = parse "(lambda (x y) (+ x y))"
        env            = Environment []
        (closure, _)   = evaluate input env
        expectedParams = DiyList [ DiySymbol "x", DiySymbol "y" ]
        expectedBody   = DiyList [ DiySymbol "+", DiySymbol "x", DiySymbol "y" ]

    assertIsClosure closure
    assertClosureFunction closure expectedParams expectedBody


lambdaArgsAreLists :: TestTree
lambdaArgsAreLists = testCase
  "\n Test 5.4 - Lambda arguments are lists. \n\
  \ The parameters of a `lambda` should be a list. \n\
  \ If not, then an error should be produced" $ do

    let input       = parse "(lambda not-a-list (+ x y))"
        env         = Environment []
        expected    = DiyError InvalidArgument
        (result, _) = evaluate input env

    assertEqual "Invalid if lambda params is not a list" expected result


lambdaExpectsTwoArgs :: TestTree
lambdaExpectsTwoArgs = testCase
  "\n Test 5.5 - Lambda expects two arguments. \n\
  \ The `lambda` form should produce an error if it's not \n\
  \ given exactly two arguments" $ do

    let input       = parse "(lambda (foo) (bar) (baz))"
        env         = Environment []
        expected    = DiyError InvalidArgument
        (result, _) = evaluate input env

    assertEqual "Invalid if not exactly two arguments" expected result


lambdaShouldNotEvaluateBody :: TestTree
lambdaShouldNotEvaluateBody = testCase
  "\n Test 5.6 - Lambda should not evaluate body. \n\
  \ The function body should not be evaluated when the lambda \n\
  \ is defined. \n\
  \ The call to `lambda` should return a function closure \n\
  \ holding the function body. The body should not be evaluated \n\
  \ before the function is called" $ do

    let input        = parse "(lambda (x) (body ((that) would never) work))"
        env          = Environment []
        (closure, _) = evaluate input env

    assertIsClosure closure


-- Now that we have the `lambda` form implemented, let's see if we can call some functions.
-- When evaluating ASTs which are lists, if the first element isn't one of the special forms
-- which we have been working with so far, it is a function call. The first element of the
-- list is the function name, and the rest of the elements are the arguments.


evaluateCallToClosure :: TestTree
evaluateCallToClosure = testCase
  "\n Test 5.7 - Evaluating a call to a closure. \n\
  \ When evaluating a closure with no arguments or free \n\
  \ variables, simply evaluate and return the function body" $ do

    let key = "my-missing-val"
        env = Environment []

    assertEvaluate env (DiySymbol key, DiyError $ LookUpError key)


evaluatingDefine :: TestTree
evaluatingDefine = testCase
  "\n Test 4.8 - Evaluating the `define` form. \n\
  \ The `define` form is used to define new bindings in the environment. \n\
  \ It should result in a change in the environment. What you return from \n\
  \ return from evaluating the definition is not important (although it \n\
  \ affects what is printed in the REPL)" $ do

    let (key, val)  = ("x", 1000)
        input       = parse $ "(define " ++ key ++ " " ++ show val ++ ")"
        oldEnv      = Environment []
        (_, newEnv) = evaluate input oldEnv

    assertLookUp newEnv key $ DiyInt val


evaluatingDefineWithWrongNumberOfArgs :: TestTree
evaluatingDefineWithWrongNumberOfArgs = testCase
  "\n Test 4.9 - Evaluating `define` with wrong number of arguments. \n\
  \ Defines should produce an error, if not given exaclty two arguments. \n\
  \ This type of check could benefit the other forms we implement as \n\
  \  well, and you might want to add them elsewhere. However, it gets \n\
  \  tiresome to keep testing for this, so the tests won't require you to" $ do

    let env      = Environment []
        expected = DiyError InvalidArgument

    assertEvaluate env (parse "(define x)", expected)
    assertEvaluate env (parse "(define x 1 2)", expected)


evaluatingDefineWithNonSymbolAsKey :: TestTree
evaluatingDefineWithNonSymbolAsKey = testCase
  "\n Test 4.10 - Evaluating `define` with a non-symbol as key. \n\
  \ Defines require the first argument to be a symbol" $ do

    let env = Environment []

    assertEvaluate env (parse "(define #t 42)", DiyError InvalidArgument)


evaluatingDefineWithExpressionAsArg :: TestTree
evaluatingDefineWithExpressionAsArg = testCase
  "\n Test 4.11 - Evaluating `define` with an expresion as argument. \n\
  \ Defines should evaluate the argument before storing it in \n\
  \ the environment" $ do

    let oldEnv      = Environment []
        key         = "x"
        input       = parse $ "(define " ++ key ++ " (+ 1 41))"
        (_, newEnv) = evaluate input oldEnv

    assertLookUp newEnv key $ DiyInt 42


evaluatingWithLookupAfterDefine :: TestTree
evaluatingWithLookupAfterDefine = testCase
  "\n Test 4.12 - Evaluating with lookup after `define`. \n\
  \ Should look up the value defined in the previous call" $ do

    let oldEnv      = Environment []
        key         = "foo"
        input       = parse $ "(define " ++ key ++ " (+ 2 2))"
        (_, newEnv) = evaluate input oldEnv

    assertEvaluate newEnv (parse "foo", DiyInt 4)



assertIsClosure :: DiyAST -> Assertion
assertIsClosure (DiyClosure _ _) = assertBool "is a closure" True
assertIsClosure exp              = assertFailure $ show exp ++ " is not a closure"

assertClosureFunction :: DiyAST -> DiyAST -> DiyAST -> Assertion
assertClosureFunction closure expectedParams expectedBody = do
  assertEqual "closure function params" expectedParams $ params closure
  assertEqual "closure function body" expectedBodyParams $ body closure


addingFunctionsToTheMixTests :: TestTree
addingFunctionsToTheMixTests =
  testGroup "- Adding functions to the mix -"
    [ lambdaEvaluatesToClosure
    , closureKeepsCopyOfEnvironment
    , closureHoldsFunction
    , lambdaArgsAreLists
    , lambdaExpectsTwoArgs
    , lambdaShouldNotEvaluateBody
    , evaluatingUndefinedSymbol
    , evaluatingDefine
    , evaluatingDefineWithWrongNumberOfArgs
    , evaluatingDefineWithNonSymbolAsKey
    , evaluatingDefineWithExpressionAsArg
    , evaluatingWithLookupAfterDefine
    ]
