# Tree swing

Tree swing is a language agnostic refactoring library which goal is to give as many refactorings as possible with the least amount of work.

Core concepts are composed refactorings or "refactoring recipes" and integrate how you like.

Many refactorings can be created using other refactorings. If you don't want to supply a more "efficient" implementation, using these recipes should work for most cases. One example is Rename which is composed of extract and inline.

The core library don't care about how you parse your language or how to serialize the transformation. There should be tools for the common use case but if you need to you should be able to build your own backends.

## Name

tree swing gets its name from the syntax tree of source code, and how it swings it around untill it looks like you want it to. It is heavily inspired by tree-sitter which is a parsing library generator. 


