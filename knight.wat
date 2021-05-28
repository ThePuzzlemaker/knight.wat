(module
    ;; === WASI Imports === ;;

    ;; fn fd_write(
    ;;     fd: u32,
    ;;     ciovecs: *const ciovec,
    ;;     n_ciovecs: u32,
    ;;     n_written: *mut usize
    ;; ) -> errno;
    ;; Write to a file descriptor.
    ;; Note: this is similar to `writev` in POSIX.
    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
    
    ;; === WeeAlloc Shim === ;;

    ;; Import the memory that rustc sets up and that weealloc uses
    (import "weealloc_shim" "memory" (memory 0))

    ;; Re-export memory to work with the WASI ABI
    (export "memory" (memory 0))

    ;; fn malloc(size: usize, align: usize) -> *mut u8;
    ;; Allocate a block of memory.
    ;;
    ;; # Safety
    ;;
    ;; - `align` must not be zero,
    ;; - `align` must be a power of two
    ;; - `size`, when rounded up to the nearest multiple of `align`, must not
    ;;    overflow (i.e., the rounded value must be less than or equal to
    ;;    `usize::MAX`),
    ;; - `size`, when rounded up to the nearest multiple of `align`, must not
    ;;    be zero,
    ;; - the allocated block of memory may or may not be initialized
    ;;
    ;; # Errors
    ;;
    ;; A null pointer indicates that memory could not be allocated.
    (import "weealloc_shim" "malloc" (func $malloc (param i32 i32) (result i32)))

    ;; fn dealloc(ptr: *mut u8, size: usize, align: usize)
    ;; Deallocate a block of memory.
    ;;
    ;; # Safety
    ;;
    ;; - `align` must not be zero,
    ;; - `align` must be a power of two
    ;; - `size`, when rounded up to the nearest multiple of `align`, must not
    ;;   overflow (i.e., the rounded value must be less than or equal to
    ;;   `usize::MAX`),
    ;; - `size`, when rounded up to the nearest multiple of `align`, must not
    ;;   be zero,
    ;; - `ptr` must be a pointer to a block of memory currently allocated by
    ;;   weealloc
    ;; - `align` and `size` must be the same values as what was used to allocate
    ;;   the block of memory
    (import "weealloc_shim" "dealloc" (func $dealloc (param i32 i32 i32)))

    ;; === Helper Functions === ;;

    ;; fn print(str: *const u8, len: u32) -> errno;
    ;; Print a string to stdout.
    ;;
    ;; # Safety
    ;;
    ;; - `str` must be a valid, well-aligned pointer to a valid UTF-8 string
    ;; - `len` must be a valid length such that reading that length starting
    ;;   from the `str` pointer does not go out-of-bounds or reach invalid or
    ;;   uninitialized data
    (func $print (param $str i32) (param $len i32) (result i32)
        (local $ciovec_ptr i32) (local $n_written i32)

        ;; Allocate a ciovec on the heap
        (local.set $ciovec_ptr (call $malloc
            (i32.const 8)
            (i32.const 1)))
        ;; Allocate a u32 on the heap
        (local.set $n_written (call $malloc
            (i32.const 4)
            (i32.const 1)))

        ;; Set the buffer of the ciovec to the buffer provided
        (i32.store (local.get $ciovec_ptr) (local.get $str))
        ;; Set the buffer length of the ciovec to the buffer length provided
        (i32.store (block (result i32)
            (local.get $ciovec_ptr)
            (i32.const 4)
            (i32.add)) (i32.const 14))

        ;; Write the data to stdout
        (call $fd_write
            (i32.const 1)
            (local.get $ciovec_ptr)
            (i32.const 1)
            (local.get $n_written))

        ;; Deallocate the ciovec array, as we don't need it anymore
        (call $dealloc
            (local.get $ciovec_ptr)
            (i32.const 8)
            (i32.const 1))
        
        ;; Deallocate the n_written variable, which is not returned but is
        ;; required to be passed by fd_write
        (call $dealloc
            (local.get $n_written)
            (i32.const 4)
            (i32.const 1))
    )

    ;; === Start === ;;

    (func $main (export "_start")
        ;; Print the hello world string
        (call $print
            (i32.const 0)
            (i32.const 14))
        ;; Drop the error code, as we don't need it
        drop
    )

    ;; === Static Data === ;;

    ;; size: 14 bytes
    (data (i32.const 0) "Hello, world!\n")
)