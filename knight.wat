(module
    ;; === WASI Imports === ;;

    ;;; fn fd_write(
    ;;;     fd: u32,
    ;;;     ciovecs: *const ciovec,
    ;;;     n_ciovecs: u32,
    ;;;     n_written: *mut usize
    ;;; ) -> errno;
    ;;; Write to a file descriptor.
    ;;; Note: this is similar to `writev` in POSIX.
    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

    ;;; fn proc_exit(code: u32) -> !;
    ;;; Terminate the process normally. An exit code of 0 indicates successful
    ;;; termination of the program. The meanings of other values is dependent on
    ;;; the environment.
    (import "wasi_snapshot_preview1" "proc_exit" (func $proc_exit (param i32)))

    ;; === Memory Allocation === ;;

    ;;; some notes about the algorithm:
    ;;;
    ;;; explicit free list style
    ;;;
    ;;; alloc'd memory block:
    ;;; struct {
    ;;;  size: u32,
    ;;;  payload/padding: [u8; size]
    ;;;  boundary_size: u32,
    ;;; }
    ;;;
    ;;; free block:
    ;;; basically the same, except payload is:
    ;;; next: *mut u8
    ;;; prev: *mut u8
    ;;; padding: [u8; size - 2*sizeof(*mut u8)]
    ;;;
    ;;; doubly-linked (free) list
    ;;; frankly, i'm not sure if this even *needs* to be a doubly-linked list but i started that way and i'm
    ;;;  not gonna change it now since it doesn't really matter
    ;;; insertion policy: address-ordered (insert free blocks as such: addr(prev) < addr(curr) < addr(next))
    ;;; free() is max of O(n) where n is # of free blocks but that's fine
    ;;;
    ;;; malloc:
    ;;; - size = round up size to nearest multiple of 4
    ;;; - size = size + (2*i32 for meta)
    ;;; - find block (best-fit?)
    ;;; - split it
    ;;; - splice it out of the free list
    ;;; - return ptr
    ;;;
    ;;; best-fit:
    ;;; - search free list
    ;;; - find block with fewest remaining bytes
    ;;;
    ;;; free:
    ;;; - find block
    ;;; - find size
    ;;; - put it in the free list
    ;;; - coalesce
    ;;;
    ;;; alignment is in multiples of 4 for the purposes of ease-of-use
    ;;; means we can use UInt32Array if we need to, and means we can actually use a bit for "is this block free?" within the size tag
    ;;; this is useful for coalescing so the algorithm doesn't have to traverse the entire free list too many times and compare addresses
    ;;;  and stuff like that which can be fallible
    ;;; it also means we can trap double frees.
    ;;; thus, we don't waste space but still retain the usefulness of having explicit free lists and not an implicit one
    ;;; i'm too lazy to do something fancy like seglists or RBtree-based allocators, so this is the best option, tbh.
    
    ;;; fn malloc(size: usize) -> *mut u8;
    ;;; Allocate a block of memory.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `size`, when rounded up to the nearest multiple of 4, must not be zero
    ;;; - `size`, when rounded up to the nearest multiple of 4, must not overflow
    ;;; - the allocated block of memory may or may not be initialized
    ;;;
    ;;; # Errors
    ;;;
    ;;; A null pointer indicates that memory could not be allocated.
    (func $malloc (param $size i32) (result i32)
        (local $actual_size i32) (local $ptr i32)
        ;; traverse list loop
        (local $next i32) (local $best_size i32) (local $best_ptr i32) (local $curr_size i32)
        (local $memsize i32) (local $memdelta i32)
        ;; splitting/splicing blocks
        (local $temp_next i32) (local $temp_prev i32) (local $next_size i32)
        
        ;; Round up $size to the nearest multiple of 4 (alignment), which is 2^2 so we can use a power of 2 optimization
        ;; (https://stackoverflow.com/a/9194117/5460075)
        ;; (numToRound + multiple - 1) & -multiple
        ;; == (numToRound + 4 - 1) & -4
        ;; == (numToRound + 3) & -4
        (local.set $size (i32.and
            (i32.add (local.get $size) (i32.const 3))
            (i32.const -4)))

        (if (i32.eqz (local.get $size))
            (return (i32.const 0)))
        
        ;; 8 is still a multiple of 4 so we retain our alignment.
        (local.set $actual_size
            (i32.add (local.get $size) (i32.const 8)))

        ;; we make sure that the size is at least 16 as we need at least
        ;; 16 bytes for the free block, thus that's our minimum block size.
        (local.set $actual_size
            (call $i32u_max
                (local.get $actual_size)
                (i32.const 16)))

        ;; ptr = heap_base
        (local.set $ptr (global.get $heap_base))

        (block $loop_out (loop
            ;; next = ptr->next
            (local.set $next
                (i32.load (i32.add (local.get $ptr)
                                   (i32.const 4))))

            (if (i32.eqz (local.get $next)) (block
                (if (i32.eqz (local.get $best_size))
                    (block
                        ;; Our best block was not found, i.e. there wasn't a block large enough.
                        ;; This means we need to make a new block, which potentially means expanding the memory.

                        ;; We use heap_base->prev to be the beginning of the first unmanaged section.
                        ;; next = heap_base->prev
                        (local.set $next
                            (i32.load (i32.add (global.get $heap_base) (i32.const 8))))

                        (local.set $memsize
                            ;; memory.size works in page units, each page is 64 KiB (65536 bytes)
                            (i32.mul (i32.const 65536) (memory.size)))
                        

                        (if (i32.lt_u
                                (i32.sub (local.get $memsize) (local.get $next))
                                (local.get $actual_size))
                            (block
                                ;; We need to grow the memory.

                                ;; Round up to the nearest multiple of 65536 (which is 2^16, so we can use a power of two optimization)
                                ;; Then divide and subtract from the existing memory.size to get the page delta.
                                ;; (https://stackoverflow.com/a/9194117/5460075)
                                ;; (numToRound + multiple - 1) & -multiple
                                ;; == (numToRound + 65536 - 1) & -65536
                                ;; == (numToRound + 65535) & -65536

                                ;; TODO: review this. It seems janky but works, but it'll probably alloc more than we need in some rare cases.
                                ;; we probably want to check based on actual_size - (memsize - next), not just actual_size but I'll have to look later
                                (local.set $memdelta (i32.sub (i32.div_u 
                                    (i32.and
                                        (i32.add (local.get $actual_size)
                                                 (i32.const 65535))
                                        (i32.const -65536))
                                    (i32.const 65536)) (memory.size)))

                                (memory.grow (local.get $memdelta))
                                (drop)))

                        ;; next->size = actual_size;
                        (i32.store (local.get $next) (local.get $actual_size))

                        ;; next->boundary_size = actual_size;
                        (i32.store (i32.add (local.get $next)
                                            (i32.sub (local.get $actual_size) (i32.const 4)))
                                    (local.get $actual_size))
                        
                        ;; We have our new block. We don't need to splice this out of the free list as it wasn't in it, anyway.
                        ;; But we do need to mark where the heap ends now so we properly allocate next time.
                        (i32.store (i32.add (global.get $heap_base)
                                            (i32.const 8))
                                   (i32.add (local.get $next) (local.get $actual_size)))

                        ;; But we don't want our caller to overwrite our precious size metadata
                        ;; (as for overruns, there's not much we can do about that), so we add 4 to the pointer (sizeof(u32)).
                        (return (i32.add (local.get $next) (i32.const 4)))
                    )
                    ;; Our best block was found.
                    ;; But $best_ptr should already be set so we just need to get out of this loop.
                    (br $loop_out))
            ))

            ;; the LSB of the size is the "is free" bit, so we need to take that out.
            ;; we can AND ~1 to clear that bit
            ;; N.B. ~1 = -2 with two's complement
            (local.set $curr_size (i32.and (i32.load (local.get $next)) (i32.const -2)))
            (if (i32.and
                    (i32.ge_u (local.get $curr_size) (local.get $actual_size))
                    (i32.or
                        (i32.lt_u (local.get $curr_size) (local.get $best_size))
                        (i32.eqz (local.get $best_size))))
                (block
                    (local.set $best_size (local.get $curr_size))
                    (local.set $best_ptr (local.get $next))))
            (local.set $ptr (local.get $next))

            (br 0)
        ))

        ;; split the block, iff best_size - actual_size >= 16
        ;; (minimum block size, including metadata & padding)
        ;; otherwise it's not worth splitting because it wouldn't even
        ;;  be a valid free block.
        (if (i32.ge_u (i32.sub (local.get $best_size)
                               (local.get $actual_size))
                      (i32.const 16))
            (block 
                (local.set $next
                    (i32.add (local.get $best_ptr) (local.get $actual_size)))

                (local.set $next_size (i32.sub (local.get $best_size)
                                               (local.get $actual_size)))

                ;; save best_ptr->next and best_ptr->prev
                ;; (in case boundary_size of either next or best_ptr occupy that space)
                
                ;; temp_next = best_ptr->next
                (local.set $temp_next
                    (i32.load (i32.add (local.get $best_ptr)
                                       (i32.const 4))))
                
                ;; temp_prev = best_ptr->prev
                (local.set $temp_prev
                    (i32.load (i32.add (local.get $best_ptr)
                                       (i32.const 8))))

                ;; best_ptr->size = actual_size
                (i32.store (local.get $best_ptr) (local.get $actual_size))
                ;; best_ptr->boundary_size = actual_size
                (i32.store (i32.add (local.get $best_ptr)
                                    (i32.sub (local.get $actual_size)
                                             (i32.const 4)))
                           (local.get $actual_size))

                ;; next->size = next_size | 1
                (i32.store (local.get $next) (i32.or (local.get $next_size) (i32.const 1)))
                ;; next->boundary_size = next_size | 1
                (i32.store (i32.add (local.get $next)
                                    (i32.sub (local.get $next_size)
                                             (i32.const 4)))
                           (i32.or (local.get $next_size) (i32.const 1)))
                
                ;; splice this new block into of the list

                ;; next->next = temp_next
                (i32.store (i32.add (local.get $next) (i32.const 4))
                    (local.get $temp_next))

                ;; next->prev = temp_prev
                (i32.store (i32.add (local.get $next) (i32.const 8))
                    (local.get $temp_prev))

                ;; temp_prev->next = next
                (i32.store (i32.add (local.get $temp_prev) (i32.const 4))
                    (local.get $next))
                
                ;; if temp_next != null, set its prev
                (if (i32.ne (i32.const 0) (local.get $temp_next))
                        ;; temp_next->prev = next
                    (i32.store (i32.add (local.get $temp_next) (i32.const 8))
                        (local.get $next)))
                

                ;; again, point to the actual data, not the size tag
                (return (i32.add (local.get $best_ptr) (i32.const 4)))
            )
            (block
                ;; next = best_ptr->next
                (local.set $next (i32.load
                    (i32.add (local.get $best_ptr) (i32.const 4))))

                ;; temp_prev = best_ptr->prev
                (local.set $temp_prev (i32.load
                    (i32.add (local.get $best_ptr) (i32.const 8))))

                ;; set the size properly
                
                ;; best_ptr->size = best_size
                (i32.store (local.get $best_ptr) (local.get $best_size))
                ;; best_ptr->boundary_size = best_size
                (i32.store (i32.add (local.get $best_ptr)
                                    (i32.sub (local.get $best_size)
                                             (i32.const 4)))
                           (local.get $best_size))
                
                ;; splice this block out of the list

                ;; temp_prev->next = next
                (i32.store (i32.add (local.get $temp_prev)
                                    (i32.const 4))
                           (local.get $next))

                ;; next->prev = temp_prev
                (i32.store (i32.add (local.get $next)
                                    (i32.const 8))
                           (local.get $temp_prev))
                
                ;; again, point to the actual data, not the size tag
                (return (i32.add (local.get $best_ptr) (i32.const 4)))
            ))
        (unreachable)
    )

    ;;; fn dealloc(ptr: *mut u8)
    ;;; Deallocate a block of memory.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `ptr` must be a pointer to a block of memory currently allocated
    (func $dealloc (param $ptr i32)
        (local $size i32)
        ;; traverse list loop
        (local $traverse_ptr i32) (local $next i32) (local $temp i32)

        ;; Our pointer starts at the beginning of the usable data.
        ;; We need it to start at the size tag.
        (local.set $ptr (i32.sub (local.get $ptr) (i32.const 4)))

        ;; size = ptr->size
        (local.set $size (i32.load (local.get $ptr)))

        ;; if this block is already free (size & 1 != 0), then panic
        (if (i32.ne (i32.const 0) (i32.and (local.get $size) (i32.const 1)))
            (block 
                ;; TODO: proper panic
                (call $fd_write (i32.const 1)
                    (global.get $data_panic_double_free_ciovec_offset)
                    (i32.const 1)
                    (global.get $data_garbage_u32_offset))
                (drop)
                (call $proc_exit (i32.const 1))
                (unreachable)))

        (local.set $traverse_ptr (global.get $heap_base))

        (loop $loop
            ;; next = traverse_ptr->next
            (local.set $next
                (i32.load (i32.add (local.get $traverse_ptr)
                                   (i32.const 4))))
            
            ;; if next == 0 then we need to add this to the list, not splice it in.
            (if (i32.eqz (local.get $next))
                (block
                    ;; add to list
                    ;; traverse_ptr->next = ptr
                    (i32.store (i32.add (local.get $traverse_ptr)
                                        (i32.const 4))
                               (local.get $ptr))
                    
                    ;; set free bit
                    ;; ptr->size |= 1
                    (i32.store (local.get $ptr) (i32.or (local.get $size) (i32.const 1)))
                    ;; ptr->boundary_size |= 1
                    (i32.store
                        (i32.add (local.get $ptr)
                                 (i32.sub (local.get $size) (i32.const 4)))
                        (i32.or (local.get $size) (i32.const 1)))

                    ;; make sure we zero out the next ptr and set the prev ptr
                    
                    ;; ptr->next = 0
                    (i32.store (i32.add (local.get $ptr)
                                        (i32.const 4))
                               (i32.const 0))
                    
                    ;; ptr->prev = traverse_ptr
                    (i32.store (i32.add (local.get $ptr)
                                        (i32.const 8))
                               (local.get $traverse_ptr))

                    ;; coalesce.
                    (call $__coalesce (local.get $ptr))

                    (return)
                )
            )

            ;; if next > ptr, then we've found the right spot
            ;; (because of our address ordering invariant)
            ;; so we need to splice the block in.
            (if (i32.gt_u (local.get $next)
                          (local.get $ptr))
                (block
                    ;; splice block in list

                    ;; traverse_ptr->next = ptr
                    (i32.store (i32.add (local.get $traverse_ptr)
                                        (i32.const 4))
                               (local.get $ptr))

                    ;; next->prev = ptr
                    (i32.store (i32.add (local.get $next)
                                        (i32.const 8))
                               (local.get $ptr))
                    
                    ;; make sure we set next and prev accordingly
                    
                    ;; ptr->next = next
                    (i32.store (i32.add (local.get $ptr)
                                        (i32.const 4))
                               (local.get $next))
                    
                    ;; ptr->prev = traverse_ptr
                    (i32.store (i32.add (local.get $ptr)
                                        (i32.const 8))
                               (local.get $traverse_ptr))

                    ;; set free bit
                    ;; ptr->size |= 1
                    (i32.store (local.get $ptr) (i32.or (local.get $size) (i32.const 1)))
                    ;; ptr->boundary_size |= 1
                    (i32.store
                        (i32.add (local.get $ptr)
                                 (i32.sub (local.get $size) (i32.const 4)))
                        (i32.or (local.get $size) (i32.const 1)))

                    ;; coalesce.
                    (call $__coalesce (local.get $ptr))

                    (return)
                )
            )
            
            ;; elsewise, we continue.
            (local.set $traverse_ptr (local.get $next))
            (br $loop)
        )
    )

    ;;; __coalesce(ptr: *mut u8)
    ;;; **!!!INTERNAL!!! !!!DO NOT USE!!!**
    ;;; free() coalescing.
    (func $__coalesce (param $ptr i32)
        (nop)
    )


    ;;; fn realloc(ptr: *mut u8, new_size: usize) -> *mut u8;
    ;;; Shrink or grow a block of memory to the given `new_size`.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `new_size`, when rounded up to the nearest multiple of 4, must not be zero
    ;;; - `new_size`, when rounded up to the nearest multiple of 4, must not overflow
    ;;; - `ptr` must be a pointer to a block of memory currently allocated
    ;;;
    ;;; # Errors
    ;;;
    ;;; This function will return a null pointer if the reallocation failed.
    ;;; However, the old block is still valid in this case.
    ;; TODO: this could be rewritten knowing the malloc internals, in such a
    ;;  way that it would be more efficient when we can just expand the current block.
    (func $realloc (param $ptr i32) (param $new_size i32) (result i32)
        (local $new_ptr i32) (local $old_size i32)
        
        ;; actual size contains metadata, which malloc(size) accounts for itself.
        ;; subtract 8 to account for this
        (local.set $old_size (i32.sub 
            (i32.load (i32.sub (local.get $ptr) (i32.const 4)))
            (i32.const 8)))

        (local.set $new_ptr (call $malloc (local.get $new_size)))

        (if (i32.eqz (local.get $new_ptr))
            (return (i32.const 0)))
        
        (call $memcpy
            (local.get $new_ptr)
            (local.get $ptr)
            (call $i32u_min
                (local.get $old_size)
                (local.get $new_size)))
        (drop)

        (call $dealloc
            (local.get $ptr))
        
        (local.get $new_ptr)
    )

    (memory 1)

    ;;; Re-export memory to work with the WASI ABI
    (export "memory" (memory 0))

    ;; === Helper Functions === ;;

    ;;; fn print(str: *const u8, len: u32) -> errno;
    ;;; Print a string to stdout.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `str` must be a valid, well-aligned pointer to a valid UTF-8 string
    ;;; - `str` must point to valid data up to an offset of `n` bytes
    (func $print (param $str i32) (param $len i32) (result i32)
        (local $ciovec_ptr i32) (local $n_written i32)

        ;; Allocate a ciovec on the heap
        (local.set $ciovec_ptr
            (call $malloc (i32.const 8)))

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
            ;; We use the `garbage_u32` for n_written as we need a valid pointer, but we don't care about the data.
            ;; and garbage_u32 is just for this purpose: useless data that's only written, never read.
            (global.get $data_garbage_u32_offset))

        ;; Deallocate the ciovec array, as we don't need it anymore
        (call $dealloc
            (local.get $ciovec_ptr))
    )

    ;;; fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32;
    ;;; This function emulates libc's memcmp.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `s1` and `s2` must be valid well-aligned pointers
    ;;; - `s1` and `s2` must point to valid data up to an offset of `n` bytes
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
                (block
                    (i32.sub (local.get $a) (local.get $b))
                    (br 2)))

            ;; Increment i
            (local.set $i
                (i32.add (local.get $i) (i32.const 1)))

            ;; Continue loop
            (br 0)
        ))

        ;; Return `0` (equal) elsewise
        (i32.const 0)
    )

    ;;; fn memcpy(dest: *const u8, src: *const u8, n: usize) -> *mut u8;
    ;;; This function emulates libc's memcpy. The returned buffer is just `dest`.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `dest` and `src` must be valid well-aligned pointers
    ;;; - `dest` and `src` must point to valid data up to an offset of `n` bytes
    (func $memcpy (param $dest i32) (param $src i32) (param $n i32) (result i32)
        (memory.copy
            (local.get $dest)
            (local.get $src)
            (local.get $n))
        (local.get $dest)
    )
    
    ;;; fn memdup(src: *const u8, size: usize) -> *mut u8;
    ;;; This function copies the data from `src` to a newly allocated buffer.
    (func $memdup (param $src i32) (param $size i32) (result i32)
        (call $memcpy
            (call $malloc (local.get $size))
            (local.get $src)
            (local.get $size))
    )

    ;;; fn memset(s: *mut u8, c: i32, n: usize) -> *mut u8;
    ;;; This function emulates libc's memset. The returned buffer is just `s`.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `s` must be a valid and well-aligned pointer
    ;;; - `s` must point to valid data up to an offset of `n` bytes
    (func $memset (param $s i32) (param $c i32) (param $n i32) (result i32)
        ;; memory.fill acts like memseet
        (memory.fill
            (local.get $s)
            (local.get $c)
            (local.get $n))
        (local.get $s)
    )

    ;;; fn i32s_abs(num: i32) -> i32
    ;;; Computes the absolute value of a signed i32. May overflow.
    (func $i32s_abs (param $num i32) (result i32)
        (if (i32.lt_s (local.get $num) (i32.const 0))
            (return (i32.mul (local.get $num) (i32.const -1)))
            (return (local.get $num)))
        (unreachable)
    )

    ;;; fn i32u_min(a: i32, b: i32) -> i32
    ;;; Returns the (unsigned) minimum of a and b.
    (func $i32u_min (param $a i32) (param $b i32) (result i32)
        (select (local.get $a) (local.get $b)
                (i32.le_u (local.get $a) (local.get $b))))

    ;;; fn i32u_max(a: i32, b: i32) -> i32
    ;;; Returns the (unsigned) maximum of a and b.
    (func $i32u_max (param $a i32) (param $b i32) (result i32)
        (select (local.get $b) (local.get $a)
                (i32.le_u (local.get $a) (local.get $b))))

    ;;; fn i32s_to_string(num: i32) -> (*mut u8, usize)
    ;;; Converts a signed integer to a string, allocated on the heap by this
    ;;; function
    (func $i32s_to_string (param $num i32) (result i32 i32)
        (local $buf i32)
        (local $ptr i32)
        (local $is_negative i32)
        (local $len i32)

        ;; Allocate a buffer 11 characters long (the text length of
        ;; `-2**31 - 1`, the minimum value of an `i32`)
        (local.set $buf (call $malloc
            (i32.const 11)))

        ;; Zero out this buffer
        (call $memset
            (local.get $buf)
            (i32.const 0)
            (i32.const 11))
        (drop)

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
                    (call $i32s_abs
                        (i32.rem_s (local.get $num) (i32.const 10)))))
            
            ;; Decrement the pointer
            (local.set $ptr
                (i32.sub (local.get $ptr) (i32.const 1)))

            ;; Divide the number by 10 (signed integer division, not float
            ;; division) and then compute its absolute value
            ;; `num = abs(num / 10)`
            (local.set $num
                (call $i32s_abs
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

        ;; Reallocate buffer to the right length
        (local.set $buf (call $realloc
            (local.get $buf)
            (local.get $len)))

        ;; Return the new buffer and length
        (local.get $len)
        (local.get $buf)
    )

    ;;; fn memcat(s1: *mut u8, s1_len: usize, s2: *mut u8, s2_len: usize) -> (*mut u8, usize);
    ;;; Concatenate two buffers into a new buffer. The new buffer will be realloc()'d from the first buffer.
    ;;; The function returns the new buffer's pointer and the new length.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `s1` and `s2` must be valid, well-aligned pointers
    ;;; - `s1` must point to valid data up to an offset of `s1_len` bytes
    ;;; - `s2` must point to valid data up to an offset of `s1_len` bytes
    (func $memcat (param $s1 i32) (param $s1_len i32) (param $s2 i32) (param $s2_len i32) (result i32 i32)
        (local $buf i32) (local $len i32)
        
        ;; Calculate combined length
        (local.set $len
            (i32.add (local.get $s1_len) (local.get $s2_len)))

        ;; Grow buffer to sufficient length
        (local.set $buf (call $realloc
            (local.get $s1)
            (local.get $len)))
        
        ;; Copy second buffer
        ;; memcpy(buf + s1_len, s2, s2_len)
        (call $memcpy
            (i32.add
                (local.get $buf) (local.get $s1_len))
            (local.get $s2)
            (local.get $s2_len))
        (drop)
        
        ;; Return buffer and length
        (local.get $len)
        (local.get $buf)
    )

    ;; === Start === ;;

    (func $main (export "_start")
        (local $str i32) (local $len i32)

        (local.set $str
            (call $memdup
                (global.get $data_str1_offset)
                (global.get $data_str1_size)))

        (call $memcat
            (local.get $str)
            (global.get $data_str1_size)
            (global.get $data_str2_offset)
            (global.get $data_str2_size))
        (local.set $str)
        (local.set $len)

        (call $print
            (local.get $str)
            (local.get $len))
        (drop)

        (call $dealloc
            (local.get $str))
    )

    ;; === Static Data === ;;

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

    (global $heap_base i32 (i32.const 0x100))
    (data (i32.const 0x100) "\10\00\00\00\00\00\00\00\10\01\00\00\10\00\00\00")

    ;; Some exports for debugging
    (export "malloc" (func $malloc))
    (export "dealloc" (func $dealloc))
    (export "memset" (func $memset))
    (export "memcpy" (func $memcpy))
    (export "print" (func $print))
    (export "i32u_min" (func $i32u_min))
)