# rinha-ocaml

This is a simple treewalker interpreter for the Rinha language.

The Rinha language is a language created for a compiler/interpreter competition. This was made after the competition took place. My official entry is https://github.com/ricardopieper/rinha-compiler which is made in Rust and has lots of optimization thrown at it.

Uses atdgen to parse the JSON AST file, and the rest is a simple treewalking recursive algorithm. There isn't support for any advanced features like TCO or memoization.

More at https://github.com/aripiprazole/rinha-de-compiler
