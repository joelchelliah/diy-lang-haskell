module Test_6_WorkingWithLists where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment                     (extend, lookup)
import           Evaluator                       (evaluate)
import           Parser                          (parse)
import           Prelude                         hiding (lookup)
import           TestHelper
import           Types


creatingListsByQuoting :: TestTree
creatingListsByQuoting = testCase
  "\n Test 6.1 - Creating lists by quoting. \n\
  \ The reason we need to use `quote` here is that otherwise \n\
  \ the expression would be seen as a call to the first element \n\
  \ -- `1` in this case, which obviously isn't a function"  $ do

    let exp = parse "(1 2 3 #t)"

    assertEvaluateWithoutEnvironment (exp, exp)


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
  \ Functions should produce a <DiyError InvalidFunctionArguments> \n\
  \ error when called with the wrong number of arguments" $ do

    let defineFn = parse "(define fn (lambda (p1 p2) 'whatever))"
        (_, env) = evaluate defineFn $ Environment []
        expected = DiyError $ InvalidFunctionArguments 2 3
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


workingWithListsTests :: TestTree
workingWithListsTests =
  testGroup "- Working with lists -"
    [ creatingListsByQuoting
    , creatingListWithCons
    , creatingLongerListsWithCons
    , gettingFirstElementFromList
    , gettingFirstElementFromEmptyList
    , callingHeadOnSomethingElse
    , gettingTheTailOfList
    , gettingTheTailOfEmptyList
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
