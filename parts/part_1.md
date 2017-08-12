# Part 1: Parsing

The language we are making is an interpreted one. This means that we basically need to implement two things: a **parser** and an **evaluator**. In this first part, we will implement the parser.

The parser converts the program into something the evaluator understands. The evaluator evaluates whatever the parser produces, and returns the result:

```

            +-----------+        +-------------+
    text    |           |  AST   |             |  result
  +-------->|  parser   |+------>|  evaluator  |+-------->
            |           |        |             |
            +-----------+        +-------------+
```

The format produced by the parser is called the *Abstract Syntax Tree* (AST) of the program.

## Our AST

So what does our AST look like? Lets have a sneak peek:

```haskell
let program = "\n\
    \(define fact \n\
    \    ;; Factorial function \n\
    \    (lambda (n) \n\
    \        (if (<= n 1) \n\
    \            1 ; Factorial of 0 is 1, and we deny \n\
    \              ; the existence of negative numbers \n\
    \            (* n (fact (- n 1))))))"

in parse program


-- The generated AST data structure should look like this:
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
```

#### The AST is created as follows

- Comments are removed.
- Each type of atom is then represented as a sub type of the `DiyAST` data type.
- Symbols are represented as `DiySymbol :: String -> DiyAST`.
    + `"foo"` parses to:
    ```haskell
      DiySymbol "foo"
    ```
- The special symbols `#t` and `#f` are represented as `DiyBool :: Bool -> DiyAST`.
    + `"#t"` parses to:
    ```haskell
      DiyBool True
    ```
- Integers are represented as `DiyInt :: Int -> DiyAST`.
    + `"42"` parses to:
    ```haskell
      DiyInt 42
    ```
- List expressions are represented as `DiyList :: [DiyAST] -> DiyAST`.
    + `"(foo #f 100)"` parses to:
    ```haskell
      DiyList [ DiySymbol "foo"
              , DiyBool False
              , DiyInt 100
              ]
    ```
- Nested expressions are parsed accordingly.
    + `"((+ (- 1 2) (* 3 42)))"` parses to:
    ```haskell
    DiyList [ DiyList [ DiySymbol "+"
                      , DiyList [ DiySymbol "-"
                                , DiyInt 1
                                , DiyInt 2
                                ]
                      , DiyList [ DiySymbol "*"
                                , DiyInt 3
                                , DiyInt 42
                                ]
                      ]
            ]
    ```


## Your turn

The parsing is done in `Parser.hs`. Here, you will be implementing the following function:
```haskell
parse :: String -> DiyAST
```
A lot of the gritty work of counting parentheses and such has already been done for you in `ParserUtil.hs`, but you must still stitch everything together.
”
- Have a look at the provided functions in `util/ParserUtil.hs` before you start. These should prove useful.

- Run the tests with the following command, and hack away until the all the tests are passing:
```bash
stack test diy-lang-haskell:test-1
```

- Each test has a description, which you should probably read it if you get stuck.

## What's next?

Continue to [part 2](part_2.md) where we evaluate some simple expressions.
