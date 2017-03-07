# DIY Lang - Haskell

> `batteriesIncluded :: Maybe[Bool]`

This is a **(WIP)** Haskell port of Kjetil Valle's [diy lang tutorial/workshop](https://github.com/kvalle/diy-lang) which guides you through implementing your very own programming language, from scratch!


By the end of this tutorial you will be the proud author of a programming language, and will hopefully better understand how programming languages work  on a fundamental level.


---
## Current status
This project is still a **Work in progress** :

:white_check_mark: Parts 1-2 - **Done**  
:construction: Part 3 - **Wip**  
:soon: Parts 4-8 - **Todo**

---


## What we will be making

We will make a relatively simple, but neat language. We aim for the following features:

- A handful of datatypes (integers, booleans and symbols)
- Variables
- First class functions with lexical scoping
- That nice homemade quality feeling

We will *not* have:

- A proper type system
- Error handling
- Good performance
- And much, much more...

The language should be able to interpret the following code by the time we are done:

```lisp
(define fact
    ;; Factorial function
    (lambda (n)
        (if (eq n 0)
            1 ; Factorial of 0 is 1
            (* n (fact (- n 1))))))

;; When parsing the file, the last statement is returned
(fact 5)
```

The syntax is very similar to languages in the Lisp family. If you find the example unfamiliar, you might want to have a look at [a more detailed description of the language](parts/language.md).


## Prerequisites
Before we get started, you need to install the Haskell tool **Stack**, clone this tutorial, and run the initial setup.

#### Install Stack
We will be using this tool to install and set up everything we need for our workshop. Install **Stack** by following the [instuctions here](https://docs.haskellstack.org/en/stable/README/) based on your platform.

#### Clone this tutorial
Then go ahead and clone this project, and navigate to the project directory:
```bash
https://github.com/joelchelliah/diy-lang-haskell.git && cd diy-lang-haskell
```

#### Setup
- From inside the project directory, install the GHC compiler by running: `stack setup`.

- Then install the [stack-run](https://hackage.haskell.org/package/stack-run) package, by running `stack install stack-run`. This gives us a handy command for building and running our project.

- Finally, run the `stack-run` command, and give it some time to install and set up all the necessary dependencies.

If everything goes well, it should also build and run the project, which should output:
```haskell
ParsedSymbol "Implement this function!"
```


## A few tips

Take the time to consider the following points before starting:

- **Keep things simple**

  Don't make things more complicated than they need to be. The tests should hopefully guide you every step of the way.

- **Read the test descriptions**

  Each test has a small text describing what you are going to implement and why. Reading these should make things easier, and you might end up learning more.

- **Use the provided functions**

  Some of the more boring details are already taken care of. Take the time to look at the functions provided in the [ParserUtil.hs](util/ParserUtil.hs), and the various imports in files where you need to do some work.

- **The Haskell cheat sheet**

  If it's been a while since you last wrote some Haskell code, then you might want to flip through this [Haskell cheat sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf) for some helpful tips and tricks.

- **Description of your language**

  Read a description of the language you are going to make in [language.md](parts/language.md).



## Getting started
The workshop is split up into **eight parts**, each consisting of an introduction followed by a bunch of unit tests. It is your job to write the code that will make these tests pass. When all the tests pass, you'll have implemented that part of the language!

> Also, this [Haskell Cheet sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf) might come in handy if you are feeling a bit rusty.

**Have fun!**

- [Part 1: Parsing](parts/part_1.md)
- [Part 2: Evaluating simple expressions](parts/part_2.md)
- [Part 3: Evaluating complex expressions](parts/part_3.md)
- [Part 4: Working with variables](parts/part_4.md)
- [Part 5: Functions](parts/part_5.md)
- [Part 6: Working with lists](parts/part_6.md)
- [Part 7: Using your language](parts/part_7.md)
- [Part 8: Final touches](parts/part_8.md)
