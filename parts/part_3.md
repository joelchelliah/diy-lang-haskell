# Part 3: Evaluating complex expressions

You have now already made a simple language, which is capable of evaluating nested arithmetic expressions. It's now time to add in the ability to use control structures.

For our language, an `if` statement will suffice. The `if` takes three arguments. The first one is the predicate `p`, which is always evaluated. The second **or** third argument is then evaluated and returned depending on the value of `p`.

## Make it happen!

Go on, you know what to do.

```bash
stack test
```
<!--
## Play while you work

Remember that the REPL is a great way to play around with your language while you work on it.

```bash
./repl
→  (if (> 42 100)
…      'foo
…      'bar)
bar
```
-->
## What's next?

Go to [part 4](part_4.md), where we add environments. This will enable us to work with variables.
