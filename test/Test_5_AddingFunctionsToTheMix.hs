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

    let lambda = parse "(lambda, (), 42)"
        env    = Environment []

    assertIsClosure . fst $ evaluate lambda env


closureKeepsCopyOfEnvironment :: TestTree
closureKeepsCopyOfEnvironment = testCase
  "\n Test 5.2 - A closure copies the environment. \n\
  \ The closure should keep a copy of the environment where it was \n\
  \ defined. Once we start calling functions later, we'll need \n\
  \ access to the environment from when the function was created \n\
  \ in order to resolve all free variables" $ do

    let lambda       = parse "(lambda, (), 42)"
        env          = Environment [("foo", DiyInt 42)]
        (closure, _) = evaluate lambda env

    assertEqual "Should be the same environment" env $ localEnv closure


closureHoldsFunction :: TestTree
closureHoldsFunction = testCase
  "\n Test 5.3 - A closure holds a function. \n\
  \ The closure contains a parameter list and a function body. They \n\
  \ are represented as a <DiyFunction [DiyAST] DiyAST> in our AST" $ do

    let lambda         = parse "(lambda (x y) (+ x y))"
        (closure, _)   = evaluate lambda $ Environment []
        expectedParams = [ DiySymbol "x", DiySymbol "y" ]
        expectedBody   = [ DiySymbol "+", DiySymbol "x", DiySymbol "y" ]

    assertIsClosure closure
    assertClosureFunction closure expectedParams expectedBody


lambdaArgsAreLists :: TestTree
lambdaArgsAreLists = testCase
  "\n Test 5.4 - Lambda arguments are lists. \n\
  \ The parameters of a `lambda` should be a list. \n\
  \ If not, then an error should be produced" $ do

    let lambda      = parse "(lambda not-a-list (+ x y))"
        expected    = DiyError InvalidArgument
        (result, _) = evaluate lambda $ Environment []

    assertEqual "Invalid if lambda params is not a list" expected result


lambdaExpectsTwoArgs :: TestTree
lambdaExpectsTwoArgs = testCase
  "\n Test 5.5 - Lambda expects two arguments. \n\
  \ The `lambda` form should produce an error if it's not \n\
  \ given exactly two arguments" $ do

    let lambda      = parse "(lambda (foo) (bar) (baz))"
        expected    = DiyError InvalidArgument
        (result, _) = evaluate lambda $ Environment []

    assertEqual "Invalid if not exactly two arguments" expected result


lambdaShouldNotEvaluateBody :: TestTree
lambdaShouldNotEvaluateBody = testCase
  "\n Test 5.6 - Lambda should not evaluate body. \n\
  \ The function body should not be evaluated when the lambda \n\
  \ is defined. \n\
  \ The call to `lambda` should return a function closure \n\
  \ holding the function body. The body should not be evaluated \n\
  \ before the function is called" $ do

    let lambda       = parse "(lambda (x) (body ((that) would never) work))"
        (closure, _) = evaluate lambda $ Environment []

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

    let lambda       = parse "(lambda () (+ 1 2))"
        expected     = DiyInt 3
        (closure, _) = evaluate lambda $ Environment []

    assertEvaluateWithoutEnvironment (expected, closure)


evaluateCallToClosureWithArgs :: TestTree
evaluateCallToClosureWithArgs = testCase
  "\n Test 5.8 - Evaluating a call to a closure with arguments. \n\
  \ The function body must be evaluated in an environment \n\
  \ where the parameters are bound. \n\
  \ Create an environment where the function parameters \n\
  \ (which are stored in the closure) are bound to the actual \n\
  \ argument values in the function call. Use this environment \n\
  \ when evaluating the function body" $ do

    let lambda       = parse "(lambda (a b) (+ a b))"
        (closure, _) = evaluate lambda $ Environment []
        expected     = DiyInt 9
        input        = DiyList [ closure
                               , DiyInt 4
                               , DiyInt 5
                               ]

    assertEvaluateWithoutEnvironment (expected, input)


creatingClosureWithEnv :: TestTree
creatingClosureWithEnv = testCase
  "\n Test 5.9 - Creating a closure with an existing environment. \n\
  \ The function parameters must properly shadow the \n\
  \ outer scope's bindings. \n\
  \ When the same bindings exist in both the environment and the \n\
  \ function parameters, the function parameters must be prioritized \n\
  \ over the ones in the environment" $ do

    let lambda       = parse "(lambda (a b) (+ a b))"
        env          = Environment [ ("a", DiyInt 42)
                                   , ("b", DiySymbol "foo")
                                   ]
        (closure, _) = evaluate lambda env
        expected     = DiyInt 9
        input        = DiyList [ closure
                               , DiyInt 4
                               , DiyInt 5
                               ]

    assertEvaluateWithEnvironment env (expected, input)


callToClosureShouldEvaluateArgs :: TestTree
callToClosureShouldEvaluateArgs = testCase
  "\n Test 5.10 - Call to closure should evaluate all arguments. \n\
  \ When a function is applied, the arguments should be evaluated \n\
  \ before being bound to the parameter names" $ do

    let lambda       = parse "(lambda (a) (+ a 5))"
        (closure, _) = evaluate lambda $ Environment []
        expected     = DiyInt 25
        input        = DiyList [ closure
                               , parse "(if #f 0 (+ 10 10))"
                               ]

    assertEvaluateWithoutEnvironment (expected, input)


evaluateCallToClosureWithFreeVariables :: TestTree
evaluateCallToClosureWithFreeVariables = testCase
  "\n Test 5.11 - Call to closure with free variables. \n\
  \ The body should be evaluated in the environment from the closure. \n\
  \ The function's free variables (i.e. those not specified as part \n\
  \ of the parameter list) should be looked up in the environment \n\
  \ from where the function was defined. This is the environment \n\
  \ included in the closure. Make sure this environment is used when \n\
  \ evaluating the body" $ do

    let lambda       = parse "(lambda (x) (+ x y))"
        env          = Environment [("y", DiyInt 1)]
        (closure, _) = evaluate lambda env
        outerEnv     = Environment [("y", DiyInt 2)]
        expected     = DiyInt 1
        input        = DiyList [ closure
                               , DiyInt 0
                               ]

    assertEvaluateWithEnvironment outerEnv (expected, input)


-- Now we're able to evaluate ASTs with closures as the first element. But normally
-- the closures don't just happen to be there all by themselves. Generally we'll
-- find some expression, evaluate it to a closure, and then evaluate a new AST with
-- the closure just like we did above.
--
-- (some-exp arg1 arg2 ...) -> (closure arg1 arg2 ...) -> result-of-function-call


callingSimpleFunctionInEnvironment :: TestTree
callingSimpleFunctionInEnvironment = testCase
  "\n Test 5.12 - Calling a simple function in the environment. \n\
  \ A call to a symbol corresponds to a call to its value in the \n\
  \  environment. \n\
  \ When a symbol is the first element of the AST list, it is \n\
  \ resolved to its value in the environment (which should be a \n\
  \ function closure). An AST with the variables replaced with \n\
  \ its value should then be evaluated instead" $ do

    let defineAddFunc   = parse "(define add (lambda (x y) (+ x y)))"
        (function, env) = evaluate defineAddFunc $ Environment []

    assertIsClosure $ lookup env "add"

    let expected = DiyInt 3
        input    = parse "(add 1 2)"

    assertEvaluateWithEnvironment env (expected, input)


assertIsClosure :: DiyAST -> Assertion
assertIsClosure (DiyClosure _ _) = assertBool "is a closure" True
assertIsClosure exp              = assertFailure $ show exp ++ " is not a closure"

assertClosureFunction :: DiyAST -> DiyFunctionParams -> DiyFunctionBody -> Assertion
assertClosureFunction (DiyClosure func _) expectedParams expectedBody = do
  assertEqual "closure function params" expectedParams $ params func
  assertEqual "closure function body" expectedBody $ body func

assertEvaluateWithoutEnvironment :: (DiyAST, DiyAST) -> Assertion
assertEvaluateWithoutEnvironment (expected, input) =
  assertEqual description expected result

  where description = descEvaluate input environment
        (result, _) = evaluate input environment
        environment = Environment []

assertEvaluateWithEnvironment :: Environment -> (DiyAST, DiyAST) -> Assertion
assertEvaluateWithEnvironment env (expected, input) =
  assertEqual description expected result

  where description = descEvaluate input env
        (result, _) = evaluate input env

descEvaluate :: DiyAST -> Environment -> String
descEvaluate input env =
  "evaluate (" ++ show input ++ ") \"" ++ show env ++ "\""

addingFunctionsToTheMixTests :: TestTree
addingFunctionsToTheMixTests =
  testGroup "- Adding functions to the mix -"
    [ lambdaEvaluatesToClosure
    , closureKeepsCopyOfEnvironment
    , closureHoldsFunction
    , lambdaArgsAreLists
    , lambdaExpectsTwoArgs
    , lambdaShouldNotEvaluateBody
    , evaluateCallToClosure
    , evaluateCallToClosureWithArgs
    , creatingClosureWithEnv
    , callToClosureShouldEvaluateArgs
    , evaluateCallToClosureWithFreeVariables
    , callingSimpleFunctionInEnvironment
    , evaluatingWithLookupAfterDefine
    ]
