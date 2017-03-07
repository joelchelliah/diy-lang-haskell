# Part 5: Functions

This part is the one you might have been eagerly anticipating. It's time to add functions to our language!

Functions are created with the `lambda` form, which returns a `Closure`. So you can start by defining the `Closure` type in `src/Types.hs`.

The first few tests will guide you through implementing the `lambda` form correctly.

The next couple of tests revolve around calling functions. A function call happens when we evaluate a list in which the first element is a function closure.

Finally, we look at handling situations where function calls are performed incorrectly, and make sure that we produce appropriate errors.


## Make it happen!

This is probably the most difficult part of making the language, so don't worry if it takes a bit longer than the previous parts.

```bash
stack test
```

## What's next?

Ready for the last part of the language? In [part 6](part_6.md), we will add ways to work with lists.
