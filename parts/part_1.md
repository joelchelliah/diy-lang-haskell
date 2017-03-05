# Part 1: parsing

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
ParsedList [ ParsedSymbol "define"
           , ParsedSymbol "fact"
           , ParsedList [ ParsedSymbol "lambda"
                        , ParsedList [ ParsedSymbol "n" ]
                        , ParsedList [ ParsedSymbol "if"
                                     , ParsedList [ ParsedSymbol "<="
                                                  , ParsedSymbol "n"
                                                  , ParsedInt 1
                                                  ]
                                     , ParsedInt 1
                                     , ParsedList [ ParsedSymbol "*"
                                                  , ParsedSymbol "n"
                                                  , ParsedList [ ParsedSymbol "fact"
                                                               , ParsedList [ ParsedSymbol "-"
                                                                            , ParsedSymbol "n"
                                                                            , ParsedInt 1
                                                                            ]
                                                               ]
                                                  ]
                                     ]
                        ]
           ]
```

#### The AST is created as follows

- Comments are removed.
- Each type of atom is then represented as a sub type of the data type `Parsed`.
- Symbols are represented as `ParsedSymbol String`.
    + `"foo"` parses to:
    ```haskell
      ParsedSymbol "foo"
    ```
- The special symbols `#t` and `#f` are represented as `ParsedBool Bool`.
    + `"#t"` parses to:
    ```haskell
      ParsedBool True
    ```
- Integers are represented as `ParsedInt Int`.
    + `"42"` parses:
    ```haskell
      ParsedInt 42
    ```
- List expressions are represented as `ParsedList [Parsed]`.
    + `"(foo #f 100)"` parses to:
    ```haskell
      ParsedList [ ParsedSymbol "foo"
                 , ParsedBool False
                 , ParsedInt 100
                 ]
    ```
- Nested expressions are parsed accordingly.
    + `"((+ (- 1 2) (* 3 42)))"` parses to:
    ```haskell
    ParsedList [ ParsedList [ ParsedSymbol "+"
                            , ParsedList [ ParsedSymbol "-"
                                         , ParsedInt 1
                                         , ParsedInt 2
                                         ]
                            , ParsedList [ ParsedSymbol "*"
                                         , ParsedInt 3
                                         , ParsedInt 42
                                         ]
                            ]
               ]
    ```


## Your turn

The parsing is done in `Parser.hs`. Here, it is your job to implement the function:
```haskell
parse :: String -> Parsed
```
A lot of the gritty work of counting parentheses and such has already been done for you in `ParserUtil.hs`, but you must stitch everything together.

- Have a look at the provided functions in `util/ParserUtil.hs` before you start. These should prove useful.

- Run the tests with the following command: `stack test`, and hack away until the all tests are passing.

- Each test has a description, which you should probably read it if you get stuck.

### What's next?

Continue to [part 2](part_2.md) where we evaluate some simple expressions.
