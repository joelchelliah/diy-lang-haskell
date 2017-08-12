# Part 4: Working with variables

So far, our interpreted language is only able to deal with expressions one by one. In a real programming language, we need to be able to store intermediate results in variables, and then later, fetch them from the environment.

We will start by implementing the `Environment` type, which can be found in `src/Types.hs`. Then we'll extend our `evaluate` function in `src\Evaluator.hs` to handle:
- Expressions using defined variables.
- The creation of new variables with the `define` form.


## Make it happen!

Run the tests, and get cracking.

```bash
stack test diy-lang-haskell:test-4
```

## What's next?

In [part 5](part_5.md), we'll look at creating lexically scoped functions with the environment we've just implemented.
