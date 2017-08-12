# Part 2: Evaluating simple expressions

Now that we have the parser up and running, it's time to start working on the evaluator. We'll start with some simple expressions, such as evaluating numbers, booleans, and a few of the most basic **special forms** in the language:

- `quote` takes one argument which is returned directly (without being evaluated).
- `atom` also takes a single argument, and returns true or false depending on whether the argument is an atom.
- `eq` returns true if both of its arguments are the same atom, and false otherwise.
- The arithmetic operators (`+`, `-`, `*`, `/`, `mod` and `>`) all take two arguments, and do exactly what you would expect.

This time, your work is in the `Evaluator.hs` module, where you will be implementing:
```haskell
evaluate :: DiyAST -> String -> DiyAST
```

## Make it happen!

The following command runs the tests for part 2. You know the drill!

```bash
stack test diy-lang-haskell:test-2
```

<!--
## Play while you work

Now that we are beginning to get an interpreter going, we can start testing the results in the **read-eval-print-loop** (REPL).

Start the REPL from the command line, and try the language as we move along.

```bash
stack-run
```

Remember, you'll need to restart the REPL for it to pick up any changes you make to the language.
-->

## What's next?

Head on to [part 3](part_3.md), where the expressions we take become slightly more complex.
