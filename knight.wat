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
    ;;   overflow (i.e., the rounded value must be less than or equal to
    ;;   `usize::MAX`),
    ;; - `size`, when rounded up to the nearest multiple of `align`, must not
    ;;   be zero,
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
    ;; - `str` must point to valid data up to an offset of `n` bytes
    (func $print (param $str i32) (param $len i32) (result i32)
        (local $ciovec_ptr i32) (local $n_written i32)

        ;; Allocate a ciovec on the heap
        (local.set $ciovec_ptr
            (call $malloc (i32.const 8) (i32.const 4)))

        ;; Allocate a u32 on the heap
        (local.set $n_written
            (call $malloc
                (i32.const 4) (i32.const 1)))

        ;; Set the buffer of the ciovec to the buffer provided
        (i32.store
            (local.get $ciovec_ptr)
            (local.get $str))

        ;; Set the buffer length of the ciovec to the buffer length provided
        (i32.store
            (i32.add
                (local.get $ciovec_ptr) (i32.const 4))
            (local.get $len))

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
            (i32.const 4))
        
        ;; Deallocate the n_written variable, which is not returned but is
        ;; required to be passed by fd_write, otherwise it would be writing to
        ;; what is most likely invalid memory
        (call $dealloc
            (local.get $n_written)
            (i32.const 4)
            (i32.const 1))
    )

    ;; fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32;
    ;; This function emulates libc's memcmp.
    ;;
    ;; # Safety
    ;;
    ;; - `s1` and `s2` must be valid well-aligned pointers
    ;; - `s1` and `s2` must point to valid data up to an offset of `n` bytes
    (func $memcmp (param $s1 i32) (param $s2 i32) (param $n i32) (result i32)
        (local $i i32) (local $a i32) (local $b i32)

        ;; if s1 == s2, then the result is always `0` (equal)
        (if
            (i32.eq
                (local.get $s1) (local.get $s2))
            (return (i32.const 0)))

        (block (loop
            ;; Make sure we don't overrun the buffer provided to us
            (br_if 1
                (i32.ge_u (local.get $i) (local.get $n)))

            ;; Sign extend the current byte from s1 into a
            (local.set $a
                (i32.load8_s
                    (i32.add (local.get $s1) (local.get $i))))

            ;; Sign extend the current byte from s2 into b
            (local.set $b
                (i32.load8_s
                    (i32.add (local.get $s2) (local.get $i))))

            ;; If the two bytes are not equal, return their difference
            (if (i32.ne (local.get $a) (local.get $b))
                (i32.sub (local.get $a) (local.get $b)))

            ;; Increment i
            (local.set $i
                (i32.add (local.get $i) (i32.const 1)))

            ;; Continue loop
            (br 0)
        ))

        ;; Return `0` (equal) elsewise
        (i32.const 0)
    )

    ;; fn memmove(dest: *const u8, src: *const u8, n: usize);
    ;; This function emulates libc's memmove.
    ;;
    ;; # Safety
    ;;
    ;; - `dest` and `src` must be valid well-aligned pointers
    ;; - `dest` and `src` must point to valid data up to an offset of `n` bytes
    (func $memmove (param $dest i32) (param $src i32) (param $n i32)
        ;; memory.copy acts like memmove, as it allows overlapping buffers
        (memory.copy
            (local.get $dest)
            (local.get $src)
            (local.get $n))
    )

    ;; fn memset(s: *mut u8, c: i32, n: usize);
    ;; This function emulates libc's memset.
    ;;
    ;; # Safety
    ;;
    ;; - `s` must be a valid and well-aligned pointer
    ;; - `s` must point to valid data up to an offset of `n` bytes
    (func $memset (param $s i32) (param $c i32) (param $n i32)
        ;; memory.fill acts like memseet
        (memory.fill
            (local.get $s)
            (local.get $c)
            (local.get $n))
    )

    ;; fn abs(num: i32) -> i32
    ;; Computes the absolute value of a number. May overflow.
    (func $abs (param $num i32) (result i32)
        (if (i32.lt_s (local.get $num) (i32.const 0))
            (return (i32.mul (local.get $num) (i32.const -1)))
            (return (local.get $num)))
        (unreachable)
    )

    ;; fn i32tostr(num: i32) -> (*mut u8, usize)
    ;; Converts a signed integer to a string, allocated on the heap by this
    ;; function
    (func $i32tostr (param $num i32) (result i32 i32)
        (local $buf i32)
        (local $ptr i32)
        (local $is_negative i32)
        (local $len i32)
        (local $new_buf i32)

        ;; Allocate a buffer 11 characters long (the text length of
        ;; `-2**31 - 1`, the minimum value of an `i32`)
        (local.set $buf (call $malloc
            (i32.const 11)
            (local.get 1)))

        ;; Zero out this buffer
        (call $memset
            (local.get $buf)
            (i32.const 0)
            (i32.const 11))

        ;; Set the pointer to the end of the buffer
        (local.set $ptr
            (i32.add (local.get $buf) (i32.const 10)))

        ;; Test whether or not this number is negative
        (local.set $is_negative
            (i32.lt_s (local.get $num) (i32.const 0)))

        (block (loop
            ;; Set the index of the current pointer to the digit as ASCII
            ;; `*ptr = '0' as u8 + abs(num % 10) as u8`
            (i32.store8
                (local.get $ptr)
                (i32.add
                    (i32.const 0x30) ;; 0x30 - ASCII value of `0`
                    (call $abs
                        (i32.rem_s (local.get $num) (i32.const 10)))))
            
            ;; Decrement the pointer
            (local.set $ptr
                (i32.sub (local.get $ptr) (i32.const 1)))

            ;; Divide the number by 10 (signed integer division, not float
            ;; division) and then compute its absolute value
            ;; `num = abs(num / 10)`
            (local.set $num
                (call $abs
                    (i32.div_s (local.get $num) (i32.const 10))))

            ;; Break if the number is 0
            (br_if 1
                (i32.eqz (local.get $num)))
            
            ;; Continue loop
            (br 0)
        ))

        ;; Add a negative sign if the number was negative
        (if (i32.eq
                (local.get $is_negative) (i32.const 1))
            (i32.store8
                (local.get $ptr)
                (i32.const 0x2d)) ;; 0x2d - ASCII value of `-`
            ;; add 1 to pointer otherwise, so it doesn't read into unintialized
            ;; memory
            (local.set $ptr
                (i32.add (local.get $ptr) (i32.const 1))))

        ;; Calculate the length of the string
        (local.set $len
            (i32.sub
                (i32.add (local.get $buf) (i32.const 11))
                (local.get $ptr)))

        ;; Allocate a new buffer that is the right length
        (local.set $new_buf (call $malloc
            (local.get $len)
            (i32.const 1)))

        ;; Copy the data from the old buffer into the new buffer
        (call $memmove
            (local.get $new_buf)
            (local.get $ptr)
            (local.get $len))

        ;; Deallocate the old buffer
        (call $dealloc
            (local.get $buf)
            (i32.const 11)
            (i32.const 1))

        ;; Return the new buffer and length
        (local.get $len)
        (local.get $new_buf)
    )

    ;; fn memcat(s1: *mut u8, s1_len: usize, s2: *mut u8, s2_len: usize) -> (*mut u8, usize);
    ;; Concatenate two buffers. The result is stored on the heap and is
    ;; allocated by this function.
    ;;
    ;; # Safety
    ;;
    ;; - `s1` and `s2` must be valid, well-aligned pointers
    ;; - `s1` must point to valid data up to an offset of `s1_len` bytes
    ;; - `s2` must point to valid data up to an offset of `s1_len` bytes
    (func $memcat (param $s1 i32) (param $s1_len i32) (param $s2 i32) (param $s2_len i32) (result i32 i32)
        (local $buf i32) (local $len i32)
        
        ;; Calculate combined length
        (local.set $len
            (i32.add (local.get $s1_len) (local.get $s2_len)))

        ;; Allocate buffer of sufficient length
        (local.set $buf (call $malloc
            (local.get $len)
            (i32.const 1)))
        
        ;; Copy first buffer
        ;; memmove(buf, s1, s1_len)
        (call $memmove
            (local.get $buf)
            (local.get $s1)
            (local.get $s1_len))
        
        ;; Copy second buffer
        ;; memmove(buf + s1_len, s2, s2_len)
        (call $memmove
            (i32.add
                (local.get $buf) (local.get $s1_len))
            (local.get $s2)
            (local.get $s2_len))
        
        ;; Return buffer and length
        (local.get $len)
        (local.get $buf)
    )

    ;; === Start === ;;

    (func $main (export "_start")
        (local $str i32) (local $len i32)
        (call $memcat
            (i32.const 0)
            (i32.const 14)
            (i32.const 15)
            (i32.const 16))
        (local.set $str)
        (local.set $len)
        (call $print
            (local.get $str)
            (local.get $len))
        (drop)
        (call $dealloc
            (local.get $str)
            (local.get $len)
            (i32.const 1))
    )

    ;; === Static Data === ;;

    ;; size: 14 bytes
    (data (i32.const 0) "Hello, world!\n")
    ;; size: 16 bytes
    (data (i32.const 15) "This is a test!\n")
)