module Test_1_Parser where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Parser
import           TestHelper
import           Types


parseSymbol :: TestTree
parseSymbol = testCase
  "\n Test 1.1 - Parsing a single symbol. \n\
  \ Symbols are represented by DiySymbols in the AST. Parsing a single \n\
  \ symbol should result in an AST consisting of a <DiySymbol String>" $ do

    let input = "foo"
        expected = DiySymbol input

    assertParse (expected, input)


parseBoolean :: TestTree
parseBoolean = testCase
  "\n Test 1.2 - Parsing a single boolean. \n\
  \ Booleans are the special symbols #t and #f. In the AST \n\
  \ they are represented as <DiyBool Bool>" $

  mapM_ assertParse
    [ (DiyBool True, "#t")
    , (DiyBool False, "#f")
    ]


parseInteger :: TestTree
parseInteger = testCase
  "\n Test 1.3 - Parsing a single integer. \n\
  \ Integers are represented in the AST as <DiyInt Int>. \n\
  \ Tip: The Data.Char library has a handy `isDigit` function" $

  mapM_ assertParse
    [ (DiyInt 42, "42")
    , (DiyInt 1337, "1337")
    ]


parseListOfSymbols :: TestTree
parseListOfSymbols = testCase
  "\n Test 1.4 - Parsing a list of symbols. \n\
  \ A list is represented by a number of elements surrounded by parens. \n\
  \ <DiyList [DiyAST]> are used to represent lists as ASTs" $

  mapM_ assertParse
    [ (DiyList $ DiySymbol <$> ["foo", "bar", "baz"], "(foo bar baz)")
    , (DiyList [], "()")
    ]


parseListOfMixedTypes :: TestTree
parseListOfMixedTypes = testCase
  "\n Test 1.5 - Parsing a list of different types. \n\
  \ When parsing lists, make sure each of the sub-expressions are \n\
  \ also parsed properly" $ do

  let input    = "(foo #t 123)"
      expected = DiyList [ DiySymbol "foo"
                         , DiyBool True
                         , DiyInt 123
                         ]

  assertParse (expected, input)


parseNestedLists :: TestTree
parseNestedLists = testCase
  "\n Test 1.6 - Parsing nested lists. \n\
  \ Parsing should also handle nested lists properly" $ do

  let input    = "(foo (bar ((#t)) x) (baz y))"
      expected =
        DiyList [ DiySymbol "foo"
                , DiyList [ DiySymbol "bar"
                          , DiyList [ DiyList [ DiyBool True ] ]
                          , DiySymbol "x"
                          ]
                , DiyList [ DiySymbol "baz"
                          , DiySymbol "y"
                          ]
                ]

  assertParse (expected, input)


parseErrorWhenMissingParen :: TestTree
parseErrorWhenMissingParen = testCase
  "\n Test 1.7 - Parsing incomplete expressions. \n\
  \ A <DiyError IncompleteExpression> should be produced if \n\
  \ the given expression is missing a parenthesis" $ do

  let input    = "(foo (bar x y)"
      expected = DiyError IncompleteExpression

  assertParse (expected, input)


parseErrorWhenExtraParen :: TestTree
parseErrorWhenExtraParen = testCase
  "\n Test 1.8 - Parsing too large expressions. \n\
  \ The parse function expects to receive only one single expression. \n\
  \ A <DiyError ExpressionTooLarge> should be produced if \n\
  \ the given expression has an extra parenthesis" $ do

  let input    = "(foo (bar x y)))"
      expected = DiyError ExpressionTooLarge

  assertParse (expected, input)


parseWithExtraWhitespace :: TestTree
parseWithExtraWhitespace = testCase
  "\n Test 1.9 - Excess whitespace should be removed. \n\
  \ Tip: The `lines` and `words` functions from the standard \n\
  \ <Prelude> library might come in handy here" $ do

  let input    = "\n      (program    with   much        whitespace)\n"
      expected = DiyList [ DiySymbol "program"
                         , DiySymbol "with"
                         , DiySymbol "much"
                         , DiySymbol "whitespace"
                         ]

  assertParse (expected, input)


parseComments :: TestTree
parseComments = testCase
  "\n Test 1.10 - Parsing expressions with comments. \n\
  \ All comments should be stripped away during \n\
  \ parsing of the expression" $ do

  let input    = "\n\
        \;; this first line is a comment \n\
        \(define variable \n\
        \   ; here is another comment \n\
        \   (if #t \n\
        \       42 ; inline comment! \n\
        \       (something else)))"
      expected =
        DiyList [ DiySymbol "define"
                , DiySymbol "variable"
                , DiyList [ DiySymbol "if"
                          , DiyBool True
                          , DiyInt 42
                          , DiyList [ DiySymbol "something"
                                    , DiySymbol "else"
                                    ]
                          ]
                ]

  assertParse (expected, input)


parseLargerExamples :: TestTree
parseLargerExamples = testCase
  "\n Test 1.11 - Parsing larger examples. \n\
  \ Testing with a larger example to check that everything \n\
  \ works as expected" $ do

  let input    = "\n\
        \(define fact \n\
        \    ;; Factorial function \n\
        \    (lambda (n) \n\
        \        (if (<= n 1) \n\
        \            1 ; Factorial of 0 is 1, and we deny \n\
        \              ; the existence of negative numbers \n\
        \            (* n (fact (- n 1))))))"
      expected =
        DiyList [ DiySymbol "define"
                , DiySymbol "fact"
                , DiyList [ DiySymbol "lambda"
                          , DiyList [ DiySymbol "n" ]
                          , DiyList [ DiySymbol "if"
                                    , DiyList [ DiySymbol "<="
                                              , DiySymbol "n"
                                              , DiyInt 1
                                              ]
                                    , DiyInt 1
                                    , DiyList [ DiySymbol "*"
                                              , DiySymbol "n"
                                              , DiyList [ DiySymbol "fact"
                                                        , DiyList [ DiySymbol "-"
                                                                  , DiySymbol "n"
                                                                  , DiyInt 1
                                                                  ]
                                                        ]
                                              ]
                                    ]
                          ]
                ]

  assertParse (expected, input)


parseQuote :: TestTree
parseQuote = testCase
  "\n Test 1.12 - Parsing a quote. \n\
  \ Quoting is a shorthand syntax for calling the `quote` form. \n\
  \ Quotes are represented as lists in the AST where the first \n\
  \ element is always a <DiySymbol \"quote\"> \n\
  \ Example: \n\
  \     \"'foo\" -> DiyList [DiySymbol \"quote\", DiySymbol \"foo\"]" $ do

  let input    = "(foo 'nil)"
      expected = DiyList [ DiySymbol "foo"
                         , DiyList [ DiySymbol "quote"
                                   , DiySymbol "nil"
                                   ]
                         ]

  assertParse (expected, input)


parseNestedQuotes :: TestTree
parseNestedQuotes = testCase
  "\n Test 1.13 - Parsing nested quotes. \n\
  \ Nested quotes should also work as expected" $ do

  let input    = "''''foo"
      quote s  = DiyList [ DiySymbol "quote", s ]
      expected =
        quote . quote . quote . quote $ DiySymbol "foo"

  assertParse (expected, input)


parseCrazyQuoteCombo :: TestTree
parseCrazyQuoteCombo = testCase
  "\n Test 1.14 - Parsing a crazy quote combo. \n\
  \ One final test to see that quote expansion works" $ do

  let input = "'(this ''''(makes ''no) 'sense)"
      desc  = "unparse $ " ++ descParse input

  assertEqual desc input . unparse $ parse input


parsingTests :: TestTree
parsingTests = testGroup "- Parsing -"
  [ parseSymbol
  , parseBoolean
  , parseInteger
  , parseListOfSymbols
  , parseListOfMixedTypes
  , parseNestedLists
  , parseErrorWhenMissingParen
  , parseErrorWhenExtraParen
  , parseWithExtraWhitespace
  , parseComments
  , parseLargerExamples
  , parseQuote
  , parseNestedQuotes
  , parseCrazyQuoteCombo
  ]
