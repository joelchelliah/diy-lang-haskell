module Test_4_WorkingWithVariables where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment
import           Evaluator
import           Parser                          (parse)
import           Prelude                         hiding (lookup)
import           Types

--
-- Before we go on to evaluating programs using variables, we need
-- to implement the functionality for storing and looking them up
-- from our Environment.
-- It is time to fill in the blanks in the `Environment` type
-- located in `src/Types.hs`.
--

simpleLookup :: TestTree
simpleLookup = testCase
  "\n Test 4.1 - A simple lookup. \n\
  \ Implement the following function in `src/Evaluator.hs` \n\
  \ `lookup :: Environment -> String -> DiyAST`"  $ do

    let key = "var"
        val = DiyInt 42
        env = Environment [(key, val)]

    assertLookUp env key val


lookUpMissingVal :: TestTree
lookUpMissingVal = testCase
  "\n Test 4.2 - Looking up a missing val. \n\
  \ When looking up an undefined symbol, a <LookUpError String> \n\
  \ should be produced. The error should contain the name of the \n\
  \ undefined symbol" $ do

    let key = "my-missing-val"
        env = Environment []

    assertLookUp env key . DiyError $ LookUpError key


extendingTheEnvironment :: TestTree
extendingTheEnvironment = testCase
  "\n Test 4.3 - Extending the environment. \n\
  \ The `extend` function should return a new environment \n\
  \ extended with the given binding" $ do

    let (key1, val1) = ("foo", DiyInt 42)
        (key2, val2) = ("bar", DiyBool True)
        oldEnv       = Environment [(key1, val1)]
        newEnv       = extend oldEnv (key2, val2)

    assertLookUp newEnv key1 val1
    assertLookUp newEnv key2 val2


lookingUpDeeplyNestedVals :: TestTree
lookingUpDeeplyNestedVals = testCase
  "\n Test 4.4 - Looking up deeply nested vals" $ do

    let (key, num) = ("foo", 100)
        ext e k n  = extend e (k, DiyInt n)
        env        = ext (ext (ext (ext (Environment []) "a" 1) "b" 2) "c" 3) key num

    assertLookUp env key $ DiyInt num


extendOverwritesOldBindings :: TestTree
extendOverwritesOldBindings = testCase
  "\n Test 4.5 - Extending overwrites old bindings. \n\
  \ Extending should overwrite old bindings with the same name" $ do

    let (key, v1, v2)          = ("foo", DiyInt 1, DiyInt 2)
        oldEnv                 = Environment [(key, v1)]
        newEnv                 = extend oldEnv (key, v2)
        numBs (Environment bs) = length bs

    assertLookUp oldEnv key v1
    assertLookUp newEnv key v2
    assertEqual "The new environment should only have one binding" 1 $ numBs newEnv


-- With the `Environment` working, it's time to implement evaluation
-- of expressions with variables.


evaluatingSymbol :: TestTree
evaluatingSymbol = testCase
  "\n Test 4.6 - Evaluating a symbol. \n\
  \ Symbols are treated as variable references. \n\
  \ When evaluating a symbol, the corresponding value \n\
  \ should be looked up in the environment" $ do

    let (key, val) = ("foo", DiyInt 42)
        env        = Environment [(key, val)]

    assertEvaluate env (DiySymbol key, val)


evaluatingUndefinedSymbol :: TestTree
evaluatingUndefinedSymbol = testCase
  "\n Test 4.7 - Evaluating an undefined symbol. \n\
  \ Referencing undefined values should produce a <LookUpError String>.\n\
  \ This test should already be working if you have implemented \n\
  \ the environment correctly" $ do

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



assertLookUp :: Environment -> String -> DiyAST -> Assertion
assertLookUp env key expected =
  assertEqual ("lookup " ++ show env ++ " " ++ show key) expected result

  where result = lookup env key

assertEvaluate :: Environment -> (DiyAST, DiyAST) -> Assertion
assertEvaluate env (input, expected) =
  assertEqual description expected result

  where description = desc input env
        (result, _) = evaluate input env

desc :: DiyAST -> Environment -> String
desc input env =
  "evaluate (" ++ show input ++ ") \"" ++ show env ++ "\""

workingWithVariabesTests :: TestTree
workingWithVariabesTests =
  testGroup "- Working with variables -"
    [ simpleLookup
    , lookUpMissingVal
    , extendingTheEnvironment
    , lookingUpDeeplyNestedVals
    , extendOverwritesOldBindings
    , evaluatingSymbol
    , evaluatingUndefinedSymbol
    , evaluatingDefine
    , evaluatingDefineWithWrongNumberOfArgs
    , evaluatingDefineWithNonSymbolAsKey
    , evaluatingDefineWithExpressionAsArg
    , evaluatingWithLookupAfterDefine
    ]
