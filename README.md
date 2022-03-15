# knight.wat

A (very) WIP implementation of [Knight][kn] in [WAT][wat]+[WASI][wasi] because I love pain

[kn]: https://github.com/knight-lang/knight
[wat]: https://webassembly.github.io/spec/core/text/index.html
[wasi]: https://github.com/WebAssembly/WASI

## Instructions

You will need to have [`cargo-make`][cargo-make] and [`wasmtime`][wasmtime] installed.
(This will obviously require Rust to be installed previously, as well, which can be done with [Rustup][rustup])

```shell
# Compile
$ makers build
# Run
$ makers run
# Clean
$ makers clean
```

## `wat-datagen`

A small program to turn binary files into a series of WAT `data` instructions with comments for name and size.

This is set up in the [`Makefile.toml`](Makefile.toml) to automatically update the data definitions at the end of [`knight.wat`](knight.wat) based on the contents of [`data/`](data/).

In the following example, the `data/` directory looks like such:
```
data
├── 00.panic_double_free_msg
├── 01.panic_double_free_ciovec
├── 10.str1
├── 11.str2
└── 99.garbage_u32
```

This structure (along with the data in the files) produces the following output:
```wasm
;;DATA BEGIN;;
    ;;; name: panic_double_free_msg
    ;;; size: 0x18
    (global $data_panic_double_free_msg_offset i32 (i32.const 0x00))
    (global $data_panic_double_free_msg_size i32 (i32.const 0x18))
    (data (i32.const 0x00) "panic: double dealloc()\n")

    ;;; name: panic_double_free_ciovec
    ;;; size: 0x08
    (global $data_panic_double_free_ciovec_offset i32 (i32.const 0x18))
    (global $data_panic_double_free_ciovec_size i32 (i32.const 0x08))
    (data (i32.const 0x18) "\00\00\00\00\18\00\00\00")

    ;;; name: str1
    ;;; size: 0x0e
    (global $data_str1_offset i32 (i32.const 0x20))
    (global $data_str1_size i32 (i32.const 0x0e))
    (data (i32.const 0x20) "Hello, world!\n")

    ;;; name: str2
    ;;; size: 0x10
    (global $data_str2_offset i32 (i32.const 0x30))
    (global $data_str2_size i32 (i32.const 0x10))
    (data (i32.const 0x30) "This is a test!\n")

    ;;; name: garbage_u32
    ;;; size: 0x04
    (global $data_garbage_u32_offset i32 (i32.const 0x40))
    (global $data_garbage_u32_size i32 (i32.const 0x04))
    (data (i32.const 0x40) "\00\00\00\00")

    (global $data_end i32 (i32.const 0x44))
;;DATA END;;
```

Note that the names are based on the extensions (as that was the easiest way to give ordering, by just having `NN.name` and then that will be alphabetically sorted into the inputs of `wat-datagen` in `Makefile.toml` through the shell)

For convenience, each entry is padded to a multiple of 4 (as WASM is 32-bit).

## wasm-runner-js

Serve this directory with a web server allowing symlinks, make sure `knight.wasm` is built, and then navigate to `index.html`. This has a button "Run!" which will, well, run the WASM. It also has a button "Dump Memory" which will download a memory dump to your computer.

It also has a built-in WASI shim for what knight.wat uses (which will extend in the future) so that it can be run and debugged in a web environment (as I've found that Chrome's debugging tools are quite good for WASM).

[cargo-make]: https://github.com/sagiegurari/cargo-make
[wasmtime]: https://github.com/bytecodealliance/wasmtime
[rustup]: https://rustup.rs