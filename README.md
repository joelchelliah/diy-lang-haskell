# DIY Lang - Haskell

> batteries not included...

This is a **(WIP)** Haskell port of Kjetil Valle's [diy lang tutorial/workshop](https://github.com/kvalle/diy-lang) which guides you through implementing your own little programming language, from scratch!

## Current status
:exclamation: This project is a **Work in progress**, and is currently not very useful :(

:construction: **Part 1: parsing** almost done! Just missing a few more tests.

...

## Prerequisites
Before we get started, you need to install [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). There is no need to install anything else. **Stack** will install and set up everything you need, including the Haskell compiler and all project dependencies, the first time you run it.

Then go ahead and clone this repo, and let's get started!
```bash
https://github.com/joelchelliah/diy-lang-haskell.git
```

:scroll: Also, this [Haskell Cheet sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf) might come in handy if you are feeling a bit rusty.

## Getting started
The workshop is split up into eight parts, each consisting of an introduction followed by a bunch of unit tests. It is your job to write the code that will make these tests pass . When all the tests pass, you'll have implemented that part of the language!

Go ahead and run `stack build` from inside the project, and give it some time to install and set up all the necessary dependencies. Once it's finished, you can run `stack test` to run the provided tests, which will point you towards what you should be implementing next!

I will update this README with more Haskell-specific information as I flesh out this port. But for now, please go see the [README of the original tutorial/workshop](https://github.com/kvalle/diy-lang) for more general information.
