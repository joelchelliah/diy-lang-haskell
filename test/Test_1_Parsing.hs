module Test_1_Parsing where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Parser


parseSymbol :: TestTree
parseSymbol = testCase
  "\n Test 1.1 - Parsing a single symbol. \n\
  \ Symbols are represented by text strings. Parsing a single atom \n\
  \ should result in an AST consisting of a <ParsedString String>" $

    assertEqual "parse \"foo\"" (ParsedString "foo") $ parse "foo"


parseBoolean :: TestTree
parseBoolean = testCase
  "\n Test 1.2 - Parsing a single boolean. \n\
  \ Booleans are the special symbols #t and #f. In the AST \n\
  \ they are represented by a <ParsedBool Bool>" $ do

  assertEqual "parse \"#t\"" (ParsedBool True)  $ parse "#t"
  assertEqual "parse \"#f\"" (ParsedBool False) $ parse "#f"


parseInteger :: TestTree
parseInteger = testCase
  "\n Test 1.3 - Parsing a single integer. \n\
  \ Integers are represented in the AST as <ParsedInt Int>. \n\
  \ Tip: The Data.Char library has a handy <isDigit> function" $ do

  assertEqual "parse \"42\"" (ParsedInt 42)  $ parse "42"
  assertEqual "parse \"1337\"" (ParsedInt 1337)  $ parse "1337"


parseListOfSymbols :: TestTree
parseListOfSymbols = testCase
  "\n Test 1.4 - Parsing a list of symbols. \n\
  \ A list is represented by a number of elements surrounded by parens. \n\
  \ <ParsedList [Parsed]> are used to represent lists as ASTs" $ do

  let expectedList  = ParsedList $ ParsedString <$> ["foo", "bar", "baz"]
      expectedEmpty = ParsedList []

  assertEqual "parse \"(foo bar baz)\"" expectedList $ parse "(foo bar baz)"
  assertEqual "parse \"()\"" expectedEmpty $ parse "()"


parseListOfMixedTypes :: TestTree
parseListOfMixedTypes = testCase
  "\n Test 1.5 - Parsing a list of different types. \n\
  \ When parsing lists, make sure each of the sub-expressions are \n\
  \ also parsed properly" $ do

  let input    = "(foo #t 123)"
      expected = ParsedList [ ParsedString "foo"
                            , ParsedBool True
                            , ParsedInt 123
                            ]

  assertEqual ("parse " ++ input) expected $ parse "(foo #t 123)"


parseNestedLists :: TestTree
parseNestedLists = testCase
  "\n Test 1.6 - Parsing nested lists. \n\
  \ Parsing should also handle nested lists properly" $ do

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
  "\n Test 1.7 - Parsing incomplete expressions. \n\
  \ A <ParseError IncompleteExpression> should be produced if \n\
  \ the given expression is missing a paren" $ do

  let input    = "(foo (bar x y)"
      expected = ParseError IncompleteExpression

  assertEqual ("parse " ++ input) expected $ parse input


parseErrorWhenExtraParen :: TestTree
parseErrorWhenExtraParen = testCase
  "\n Test 1.8 - Parsing too large expressions. \n\
  \ The parse function expects to receive only one single expression. \n\
  \ A <ParseError ExpressionTooLarge> should be produced if \n\
  \ the given expression has an extra paren" $ do

  let input    = "(foo (bar x y)))"
      expected = ParseError ExpressionTooLarge

  assertEqual ("parse " ++ input) expected $ parse input
  assertEqual ("parse " ++ input) expected $ parse input


parseWithExtraWhitespace :: TestTree
parseWithExtraWhitespace = testCase
  "\n Test 1.9 - Excess whitespace should be removed. \n\
  \ Tip: The <lines> and <words> functions from the standard \n\
  \ <Prelude> library might come in handy here" $ do

  let input    = "\n(program    with   much        whitespace)\n"
      expected = ParsedList [ ParsedString "program"
                            , ParsedString "with"
                            , ParsedString "much"
                            , ParsedString "whitespace"
                            ]

  assertEqual ("parse " ++ input) expected $ parse input


parseComments :: TestTree
parseComments = testCase
  "\n Test 1.10 - Parsing expressions with comments. \n\
  \ All comments should be stripped away during \n\
  \ parsing of the expression" $ do

  let input    =
        ";; this first line is a comment \n\
        \(define variable \n\
        \   ; here is another comment \n\
        \   (if #t \n\
        \       42 ; inline comment! \n\
        \       (something else)))"
      expected =
        ParsedList [ ParsedString "define"
                   , ParsedString "variable"
                   , ParsedList [ ParsedString "if"
                                , ParsedBool True
                                , ParsedInt 42
                                , ParsedList [ ParsedString "something"
                                             , ParsedString "else"
                                             ]
                                ]
                   ]

  assertEqual ("parse " ++ input) expected $ parse input


parseLargerExamples :: TestTree
parseLargerExamples = testCase
  "\n Test 1.11 - Parsing larger examples. \n\
  \ Testing with a larger example to check that everything \n\
  \ works as expected" $ do

  let input    =
        "(define fact \n\
        \;; Factorial function \n\
        \(lambda (n) \n\
        \    (if (<= n 1) \n\
        \        1 ; Factorial of 0 is 1, and we deny \n\
        \          ; the existence of negative numbers \n\
        \        (* n (fact (- n 1))))))"
      expected =
        ParsedList [ ParsedString "define"
                   , ParsedString "fact"
                   , ParsedList [ ParsedString "lambda"
                                , ParsedList [ ParsedString "n" ]
                                , ParsedList [ ParsedString "if"
                                             , ParsedList [ ParsedString "<="
                                                          , ParsedString "n"
                                                          , ParsedInt 1
                                                          ]
                                             , ParsedInt 1
                                             , ParsedList [ ParsedString "*"
                                                          , ParsedString "n"
                                                          , ParsedList [ ParsedString "fact"
                                                                       , ParsedList [ ParsedString "-"
                                                                                    , ParsedString "n"
                                                                                    , ParsedInt 1
                                                                                    ]
                                                                       ]
                                                          ]
                                             ]
                                ]
                   ]

  assertEqual ("parse " ++ input) expected $ parse input


-- The following tests checks that quote expansion works properly


parseQuote :: TestTree
parseQuote = testCase
  "\n Test 1.12 - Parsing a quote. \n\
  \ Quoting is a shorthand syntax for calling the `quote` form. \n\
  \ Examples: \n\
  \     'foo -> (quote foo) \n\
  \     '(foo bar) -> (quote (foo bar))" $ do

  let input    = "(foo 'nil)"
      expected = ParsedList [ ParsedString "foo"
                            , ParsedList [ ParsedString "quote"
                                         , ParsedString "nil"
                                         ]
                            ]

  assertEqual ("parse " ++ input) expected $ parse input


parseNestedQuotes :: TestTree
parseNestedQuotes = testCase
  "\n Test 1.13 - Parsing nested quotes. \n\
  \ Nested quotes should also work as expected" $ do

  let input    = "''''foo"
      expected =
        ParsedList [ ParsedString "quote"
                   , ParsedList [ ParsedString "quote"
                                , ParsedList [ ParsedString "quote"
                                             , ParsedList [ ParsedString "quote"
                                                          , ParsedString "foo"
                                                          ]
                                             ]
                                ]
                   ]

  assertEqual ("parse " ++ input) expected $ parse input


parseCrazyQuoteCombo :: TestTree
parseCrazyQuoteCombo = testCase
  "\n Test 1.14 - Parsing a crazy quote combo. \n\
  \ One final test to see that quote expansion works" $ do

  let input    = "'(this ''''(makes ''no) 'sense)"
      expected =
        ParsedList [ ParsedString "quote"
                   , ParsedList [ ParsedString "quote"
                                , ParsedList [ ParsedString "quote"
                                             , ParsedList [ ParsedString "quote"
                                                          , ParsedString "foo"
                                                          ]
                                             ]
                                ]
                   ]

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
  , parseErrorWhenExtraParen
  , parseWithExtraWhitespace
  , parseComments
  , parseLargerExamples
  , parseQuote
  , parseNestedQuotes
  , parseCrazyQuoteCombo
  ]
