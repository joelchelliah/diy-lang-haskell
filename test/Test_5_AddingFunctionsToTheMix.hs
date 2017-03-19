module Test_5_AddingFunctionsToTheMix where

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

    let lambda = parse "(lambda () 42)"
        env    = Environment []

    assertIsClosure . fst $ evaluate lambda env


closureKeepsCopyOfEnvironment :: TestTree
closureKeepsCopyOfEnvironment = testCase
  "\n Test 5.2 - A closure copies the environment. \n\
  \ The closure should keep a copy of the environment where it was \n\
  \ defined. Once we start calling functions later, we'll need \n\
  \ access to the environment from when the function was created \n\
  \ in order to resolve all free variables" $ do

    let lambda       = parse "(lambda () 42)"
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
        expectedBody   = DiyList [ DiySymbol "+", DiySymbol "x", DiySymbol "y" ]

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


callingLambdasDirectly :: TestTree
callingLambdasDirectly = testCase
  "\n Test 5.13 - Should be possible to define and call functions directly. \n\
  \ A lambda definition in the call position of an AST should \n\
  \  be evaluated, and then evaluated again, as before" $ do

    let input    = parse "((lambda (x) x) 42)"
        expected = DiyInt 42

    assertEvaluateWithoutEnvironment (expected, input)


callingComplexExpressionWhichEvaluatesToFunction :: TestTree
callingComplexExpressionWhichEvaluatesToFunction = testCase
  "\n Test 5.14 - Calling a complex expression that evaluates to a function. \n\
  \ ASTs that are lists beginning with anything except atoms or symbols, \n\
  \ should be evaluated and then called. \n\
  \ Here, a call is done to the if-expression. The `if` should be evaluated, \n\
  \ which results in a `lambda` expression. The lambda should be evaluated, \n\
  \ giving a closure. The result is an AST with a closure as the first \n\
  \ element, which we already know how to evaluate" $ do

    let env      = Environment [("y", DiyInt 3)]
        expected = DiyInt 5
        input    = parse "\n\
        \((if #f \n\
        \     wont-evaluate-this-branch\n\
        \     (lambda (x) (+ x y)))\n\
        \ 2)"

    assertEvaluateWithEnvironment env (expected, input)


-- Now that we have the happy cases working, let's see what should happen when
-- function calls are done incorrectly.


callingAnAtomProducesError :: TestTree
callingAnAtomProducesError = testCase
  "\n Test 5.15 - Calling an atom is just not right. \n\
  \ A function call to a non-function should produce a \n\
  \ <DiyError NotAFunction> error" $ do

    let expected = DiyError NotAFunction

    mapM_ assertEvaluateWithoutEnvironment
      [ (expected, parse "(#t 'foo 'bar)")
      , (expected, parse "(42)"          )
      ]


argumentsToFunctionsAreEvaluated :: TestTree
argumentsToFunctionsAreEvaluated = testCase
  "\n Test 5.16 - Arguments passed to functions should be evaluated. \n\
  \ We should accept parameters that are produced through function \n\
  \ calls. Check if you are properly evaluating the passed \n\
  \ function arguments" $ do

    let expected = DiyInt 3
        input    = parse "((lambda (x) x) (+ 1 2))"

    assertEvaluateWithoutEnvironment (expected, input)


callingWithWrongNumberOfArguments :: TestTree
callingWithWrongNumberOfArguments = testCase
  "\n Test 5.17 - Calling with wrong number of arguments. \n\
  \ Functions should produce a <DiyError WrongNumberOfFunctionArguments> \n\
  \ error when called with the wrong number of arguments" $ do

    let defineFn = parse "(define fn (lambda (p1 p2) 'whatever))"
        (_, env) = evaluate defineFn $ Environment []
        expected = DiyError $ WrongNumberOfFunctionArguments 2 3
        input    = parse "(fn 1 2 3)"

    assertEvaluateWithEnvironment env (expected, input)


callingNothing :: TestTree
callingNothing = testCase
  "\n Test 5.18 - Calling nothing should fail. \n\
  \ Calling nothing at all should produce the error: \n\
  \ <DiyError EmptyFunctionCall> \n\
  \ Remember to quote empty data lists" $ do

    let expected = DiyError EmptyFunctionCall
        input    = parse "()"

    assertEvaluateWithoutEnvironment (expected, input)


argumentsAreEvaluatedInCorrectEnvironment :: TestTree
argumentsAreEvaluatedInCorrectEnvironment = testCase
  "\n Test 5.19 - Arguments are evaluated in the correct environment. \n\
  \ Function arguments should be evaluated in the environment \n\
  \ where the function is called, and not in the environment captured \n\
  \ by the function" $ do

    let defineFoo  = parse "(define foo (lambda (x) x))"
        (foo, env) = evaluate defineFoo $ Environment [("x", DiyInt 3)]
        newEnv     = extend env ("x", DiyInt 4)
        expected   = DiyInt 5
        input      = parse "(foo (+ x 1))"

    assertEvaluateWithEnvironment newEnv (expected, input)


-- One final test to see that recursive functions are working as expected.
-- This should already be working by now! :)


callingFunctionRecursively :: TestTree
callingFunctionRecursively = testCase
  "\n Test 5.20 - Calling a function recursively. \n\
  \ A named function should be included in the environment \n\
  \ where it is evaluated" $ do

    let defineFn  = parse "\n\
    \(define my-fn\n\
    \        ;; A meaningless, but recursive, function\n\
    \        (lambda (x)\n\
    \            (if (eq x 0)\n\
    \                42\n\
    \                (my-fn (- x 1)))))"
        (fn, env) = evaluate defineFn $ Environment []

    mapM_ (assertEvaluateWithEnvironment env)
      [ (DiyInt 42, parse "(my-fn 0)")
      , (DiyInt 42, parse "(my-fn 10)")
      ]


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
    , callingLambdasDirectly
    , callingComplexExpressionWhichEvaluatesToFunction
    , callingAnAtomProducesError
    , argumentsToFunctionsAreEvaluated
    , callingWithWrongNumberOfArguments
    , callingNothing
    , argumentsAreEvaluatedInCorrectEnvironment
    , callingFunctionRecursively
    ]
