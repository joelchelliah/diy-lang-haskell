module Test_1_Parsing where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Parser


parseSymbol :: TestTree
parseSymbol = testCase
  "\n Test 1.1 - Parsing a single symbol: \n\
  \ Symbols are represented by text strings. Parsing a single atom \n\
  \ should result in an AST consisting of a <ParsedString String>." $

    assertEqual "parse \"foo\"" (ParsedString "foo") $ parse "foo"


parseBoolean :: TestTree
parseBoolean = testCase
  "\n Test 1.2 - Parsing a single boolean: \n\
  \ Booleans are the special symbols #t and #f. In the ASTs \n\
  \ they are represented by a <ParsedBool Bool>." $ do

  assertEqual "parse \"#t\"" (ParsedBool True)  $ parse "#t"
  assertEqual "parse \"#f\"" (ParsedBool False) $ parse "#f"


parseInteger :: TestTree
parseInteger = testCase
  "\n Test 1.3 - Parsing a single integer: \n\
  \ Integers are represented in the ASTs as <ParsedInt Int>. \n\
  \ Tip: The Data.Char library has a handy isDigit function." $ do

  assertEqual "parse \"42\"" (ParsedInt 42)  $ parse "42"
  assertEqual "parse \"1337\"" (ParsedInt 1337)  $ parse "1337"


parseListOfSymbols :: TestTree
parseListOfSymbols = testCase
  "\n Test 1.4 - Parsing a list of symbols: \n\
  \ A list is represented by a number of elements surrounded by parens. \n\
  \ <ParsedList [Parsed]> are used to represent lists as ASTs." $ do

  let expectedList  = ParsedList $ ParsedString <$> ["foo", "bar", "baz"]
      expectedEmpty = ParsedList []

  assertEqual "parse \"(foo bar baz)\"" expectedList $ parse "(foo bar baz)"
  assertEqual "parse \"()\"" expectedEmpty $ parse "()"


parseListOfMixedTypes :: TestTree
parseListOfMixedTypes = testCase
  "\n Test 1.5 - Parsing a list of different types: \n\
  \ When parsing lists, make sure each of the sub-expressions are \n\
  \ also parsed properly." $ do

  let input    = "(foo #t 123)"
      expected = ParsedList [ ParsedString "foo"
                            , ParsedBool True
                            , ParsedInt 123
                            ]

  assertEqual ("parse " ++ input) expected $ parse "(foo #t 123)"


parseNestedLists :: TestTree
parseNestedLists = testCase
  "\n Test 1.6 - Parsing nested lists: \n\
  \ Parsing should also handle nested lists properly." $ do

  let input    = "(foo (bar ((#t)) x) (baz y))"
      expected =
        ParsedList [ ParsedString "foo"
                   , ParsedList [ ParsedString "bar"
                                , ParsedList [ ParsedList [ ParsedBool True ] ]
                                , ParsedString "x"
                                ]
                   , ParsedList [ ParsedString "baz"
                                , ParsedString "y"
                                ]
                   ]

  assertEqual ("parse " ++ input) expected $ parse input


parseErrorWhenMissingParen :: TestTree
parseErrorWhenMissingParen = testCase
  "\n Test 1.7 - Parsing incomplete expressions: \n\
  \ A <ParseError IncompleteExpression> should be produced if \n\
  \ the given expresions is missing a paren." $ do

  let input    = "(foo (bar x y)"
      expected = ParseError IncompleteExpression

  assertEqual ("parse " ++ input) expected $ parse input


parsingTests :: TestTree
parsingTests = testGroup "Parsing"
  [ parseSymbol
  , parseBoolean
  , parseInteger
  , parseListOfSymbols
  , parseListOfMixedTypes
  , parseNestedLists
  , parseErrorWhenMissingParen
  ]
