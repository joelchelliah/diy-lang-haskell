# Part 6: Working with lists

This is the final part of implementing the language, and compared to the previous part, it should be relatively easy.

Any proper language needs good data structures, and **lists** are one of them. To be able to work properly with lists, we’ll introduce four new forms to our language:

- `cons` is used to construct lists from a **head**, which is a single element, and a **tail**, which is the rest of the list.
- `head` extracts the first element of a list.
- `tail` drops the first element, and returns the rest of the list.
- `empty` takes a list as input, and returns `#t` if it is empty and `#f` otherwise.


## Make it happen!

Go on then, finish your language.

```bash
stack test
```

## What's next?

With the language implementation done, it's time to start using our language in [part 7](part_7.md).
