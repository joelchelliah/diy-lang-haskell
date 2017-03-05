# DIY Lang - Haskell

> `batteriesIncluded :: Maybe[Bool]`

This is a **(WIP)** Haskell port of Kjetil Valle's [diy lang tutorial/workshop](https://github.com/kvalle/diy-lang) which guides you through implementing your own little programming language, from scratch!

## Current status
:exclamation: This project is a **Work in progress**, and is currently not very useful :(

:construction: **Part 1: parsing** almost done! Just missing a few more tests.

...

## Prerequisites
Before we get started, you need to install the Haskell tool **Stack**, clone the workshop repo, and run the initial set up.

#### Install Stack
We will be using this tool to install and set up everything we need for our workshop.
* Check out their [README](https://docs.haskellstack.org/en/stable/README/) for installation information.
* Install the [stack-run](https://hackage.haskell.org/package/stack-run) package which gives us a handy command for building and running our project.

#### Clone the workshop
Then go ahead and clone this repository:
```bash
https://github.com/joelchelliah/diy-lang-haskell.git
```

#### Setup
Finally, run the `stack-run` command from inside the project directory, and give it some time to install and set up all the necessary dependencies.

If everything goes well, it should also build and run the project, which should output: `ParsedString "Yay input!"`.


## Getting started
The workshop is split up into eight parts, each consisting of an introduction followed by a bunch of unit tests. It is your job to write the code that will make these tests pass . When all the tests pass, you'll have implemented that part of the language!

Run `stack test` to run the provided tests, which will point you towards what you should be implementing next!

:scroll: This [Haskell Cheet sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf) might come in handy if you are feeling a bit rusty.

I will update this README with more Haskell-specific information as I flesh out this port. But for now, please go see the [README of the original tutorial/workshop](https://github.com/kvalle/diy-lang) for more general information.
