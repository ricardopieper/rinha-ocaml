# rinha-ocaml

This is a simple treewalker interpreter for the Rinha language.

The Rinha language is a language created for a compiler/interpreter competition. This was made after the competition took place. My official entry is https://github.com/ricardopieper/rinha-compiler which is made in Rust and has lots of optimization thrown at it.

Uses atdgen to parse the JSON AST file, and the rest is a simple treewalking recursive algorithm. There isn't support for any advanced features like TCO or memoization.

More at https://github.com/aripiprazole/rinha-de-compiler

# Running

This enables the Flambda compiler with some other options, I copy pasted from the 5.1.0 changelog 💀

It's not that much faster, so maybe you can ignore this.

```
opam switch create 5.1.0+flambda+nffa ocaml-variants.5.1.0+options ocaml-option-flambda ocaml-option-no-flat-float-array
```

Then run `opam install . --deps-only` to install dependencies.

You also need to run `cargo add rinha` as this is the project's parser.

To run: `rinha fib.rinha > out.json && dune exec bin/main.exe`

