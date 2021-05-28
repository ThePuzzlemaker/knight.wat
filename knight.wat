(module
    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
    (import "weealloc_shim" "memory" (memory 0))
    (import "weealloc_shim" "malloc" (func $malloc (param i32 i32) (result i32)))
    (import "weealloc_shim" "dealloc" (func $dealloc (param i32 i32 i32)))
    (export "memory" (memory 0))

    ;; size: 14 bytes
    (data (i32.const 0) "Hello, world!\n")    

    ;; fn print(str: *const u8, len: u32) -> errno;
    ;; safety: str and len must be valid
    (func $print (param $str i32) (param $len i32) (result i32)
        (local $ciovec_ptr i32) (local $n_written i32)
        (local.set $ciovec_ptr (call $malloc
            (i32.const 8)
            (i32.const 1)))
        (local.set $n_written (call $malloc
            (i32.const 4)
            (i32.const 1)))
        (i32.store (local.get $ciovec_ptr) (local.get $str))
        (i32.store (block (result i32)
            (local.get $ciovec_ptr)
            (i32.const 4)
            (i32.add)) (i32.const 14))
        (call $fd_write
            (i32.const 1)
            (local.get $ciovec_ptr)
            (i32.const 1)
            (local.get $n_written))
        (call $dealloc
            (local.get $ciovec_ptr)
            (i32.const 8)
            (i32.const 1))
        (call $dealloc
            (local.get $n_written)
            (i32.const 4)
            (i32.const 1))
    )

    (func $main (export "_start")
        (call $print
            (i32.const 0)
            (i32.const 14))
        drop
    )
)