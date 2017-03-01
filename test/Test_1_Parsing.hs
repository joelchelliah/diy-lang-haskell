module Test_1_Parsing where

import Test.Tasty
import Test.Tasty.HUnit


parseSymbol = testCase
  "\n Test 1.1 - Parsing a single symbol: \n\
  \ Symbols are represented by text strings. Parsing a single atom \n\
  \ should result in an AST consisting of only that symbol." $
  [1, 2, 3] `compare` [1,2] @?= GT

parseBoolean = testCase
  "\n Test 1.2 - Parsing a single boolean: \n\
  \ Booleans are the special symbols #t and #f. In the ASTs they \n\
  \ are represented by Haskell's True and False, respectively." $
  [1, 2, 3] `compare` [1,2,2] @?= LT

parseInteger = testCase
  "\n Test 1.3 - Parsing a single integer: \n\
  \ Integers are represented in the ASTs as Haskell Ints. \n\
  \ Tip: The Data.Char library has a handy isDigit function." $
  [1, 2, 3] `compare` [1,2,2] @?= LT

parsingTests = testGroup "Parsing"
  [ parseSymbol
  , parseBoolean
  , parseInteger
  ]
