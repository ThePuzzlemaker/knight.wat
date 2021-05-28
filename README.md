# knight.wat

A (very) WIP implementation of [Knight][kn] in [WAT][wat]+[WASI][wasi] because I love pain

[kn]: https://github.com/knight-lang/knight
[wat]: https://webassembly.github.io/spec/core/text/index.html
[wasi]: https://github.com/WebAssembly/WASI

## `weealloc-shim`

A small shim over [`wee_alloc`][weealloc] because otherwise I would have to make my own memory allocator.

[weealloc]: https://github.com/rustwasm/wee_alloc

## Instructions

You will need to have [`cargo-make`][cargo-make], [`cargo-wasi`][cargo-wasi], and [`wasmtime`][wasmtime] installed.
(This will obviously require Rust to be installed previously, as well, which can be done with [Rustup][rustup])

```shell
# Compile
$ cargo make build
# Run
$ cargo make run
# Clean
$ cargo make clean
```

[cargo-make]: https://github.com/sagiegurari/cargo-make
[cargo-wasi]: https://github.com/bytecodealliance/cargo-wasi
[wasmtime]: https://github.com/bytecodealliance/wasmtime
[rustup]: https://rustup.rs