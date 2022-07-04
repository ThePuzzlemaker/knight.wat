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

        (block $loop_out (loop $loop
            ;; next = ptr->next
            (local.set $next
                (i32.load offset=4 (local.get $ptr)))

            (if (i32.eqz (local.get $next)) (block
                (if (i32.eqz (local.get $best_size))
                    (block
                        ;; Our best block was not found, i.e. there wasn't a block large enough.
                        ;; This means we need to make a new block, which potentially means expanding the memory.

                        ;; We use heap_base->prev to be the beginning of the first unmanaged section.
                        ;; next = heap_base->prev
                        (local.set $next
                            (i32.load offset=8 (global.get $heap_base)))

                        (local.set $memsize
                            ;; memory.size works in page units, each page is 64 KiB (65536 bytes)
                            (i32.mul (i32.const 65536) (memory.size)))
                        

                        (if (i32.lt_u
                                (i32.sub (local.get $memsize) (local.get $next))
                                (local.get $actual_size))
                            (block
                                ;; We need to grow the memory.

                                ;; Round up to the nearest multiple of 65536 (which is 2^16, so we can use a power of two optimization)
                                ;; Then divide to get the page delta.
                                ;; (https://stackoverflow.com/a/9194117/5460075)
                                ;; (numToRound + multiple - 1) & -multiple
                                ;; == (numToRound + 65536 - 1) & -65536
                                ;; == (numToRound + 65535) & -65536

                                ;; TODO: review this. It seems janky but works, but it'll probably alloc more than we need in some rare cases.
                                ;; we probably want to check based on actual_size - (memsize - next), not just actual_size but I'll have to look later
                                (local.set $memdelta (i32.div_u 
                                    (i32.and
                                        (i32.add (i32.sub (local.get $actual_size)
                                                          (i32.sub (local.get $memsize) (local.get $next)))
                                                 (i32.const 65535))
                                        (i32.const -65536))
                                    (i32.const 65536)))

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
                        ;; heap_base->prev = next + actual_size;
                        (i32.store offset=8 (global.get $heap_base)
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

            (br $loop)
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
                    (i32.load offset=4 (local.get $best_ptr)))
                
                ;; temp_prev = best_ptr->prev
                (local.set $temp_prev
                    (i32.load offset=8 (local.get $best_ptr)))

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
                (i32.store offset=4 (local.get $next)
                    (local.get $temp_next))

                ;; next->prev = temp_prev
                (i32.store offset=8 (local.get $next)
                    (local.get $temp_prev))

                ;; temp_prev->next = next
                (i32.store offset=4 (local.get $temp_prev)
                    (local.get $next))
                
                ;; if temp_next != null, set its prev
                (if (i32.ne (i32.const 0) (local.get $temp_next))
                    ;; temp_next->prev = next
                    (i32.store offset=8 (local.get $temp_next)
                        (local.get $next)))
                

                ;; again, point to the actual data, not the size tag
                (return (i32.add (local.get $best_ptr) (i32.const 4)))
            )
            (block
                ;; next = best_ptr->next
                (local.set $next (i32.load offset=4 (local.get $best_ptr)))

                ;; temp_prev = best_ptr->prev
                (local.set $temp_prev (i32.load offset=8 (local.get $best_ptr)))

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
                (i32.store offset=4 (local.get $temp_prev)
                           (local.get $next))

                ;; next->prev = temp_prev
                (i32.store offset=8 (local.get $next)
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
                (call $print (global.get $data_panic_double_dealloc_offset)
                             (global.get $data_panic_double_dealloc_size))
                (drop)
                (call $proc_exit (i32.const 1))
                (unreachable)))

        (local.set $traverse_ptr (global.get $heap_base))

        (loop $loop
            ;; next = traverse_ptr->next
            (local.set $next
                (i32.load offset=4 (local.get $traverse_ptr)))
            
            ;; if next == 0 then we need to add this to the list, not splice it in.
            (if (i32.eqz (local.get $next))
                (block
                    ;; add to list
                    ;; traverse_ptr->next = ptr
                    (i32.store offset=4 (local.get $traverse_ptr)
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
                    (i32.store offset=4 (local.get $ptr)
                               (i32.const 0))
                    
                    ;; ptr->prev = traverse_ptr
                    (i32.store offset=8 (local.get $ptr)
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
                    (i32.store offset=4 (local.get $traverse_ptr)
                               (local.get $ptr))

                    ;; next->prev = ptr
                    (i32.store offset=8 (local.get $next)
                               (local.get $ptr))
                    
                    ;; make sure we set next and prev accordingly
                    
                    ;; ptr->next = next
                    (i32.store offset=4 (local.get $ptr)
                               (local.get $next))
                    
                    ;; ptr->prev = traverse_ptr
                    (i32.store offset=8 (local.get $ptr)
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
    ;;; !!!INTERNAL, DO NOT USE!!!
    ;;; This ALMOST CERTAINLY WILL corrupt your heap.
    ;;; free() coalescing.
    (func $__coalesce (param $ptr i32)
        ;; $forward_loop
        (local $next i32) (local $size i32)
        (local $temp_next i32)
        ;; $backward_loop
        (local $prev i32) (local $boundary_ptr i32)
        (local $temp_prev i32) (local $temp_size i32)

        ;; clear free bit
        ;; this will always be here as we've ensured that this block is free.
        ;; if someone calls this somehow elsewhere, that's their loss.
        ;; size = ptr->size & -2;
        (local.set $size (i32.and (i32.load (local.get $ptr))
                                  (i32.const -2)))

        (block $forward_loop_out (loop $forward_loop
            (local.set $next (i32.add (local.get $ptr) (local.get $size)))

            ;; if the next block isn't free, exit the loop.
            (br_if $forward_loop_out (i32.eqz (i32.and (i32.load (local.get $next)) (i32.const 1))))
            
            ;; if it's not, we want to coalesce with this block.

            ;; temp_next = next->next
            (local.set $temp_next (i32.load offset=4 (local.get $next)))

            ;; size += next->size & -2
            (local.set $size (i32.add (local.get $size)
                                      (i32.and (i32.load (local.get $next))
                                               (i32.const -2))))
            
            ;; ptr->size = size | 1
            (i32.store (local.get $ptr)
                       (i32.or (local.get $size) (i32.const 1)))

            ;; ptr->boundary_size = size | 1
            (i32.store (i32.add (local.get $ptr)
                                (i32.sub (local.get $size)
                                         (i32.const 4)))
                       (i32.or (local.get $size) (i32.const 1)))
            
            ;; ptr->next = temp_next
            (i32.store offset=4 (local.get $ptr)
                       (local.get $temp_next))

            ;; if temp_next != null, temp_next->prev = ptr
            (if (i32.ne (i32.const 0) (local.get $temp_next))
                (i32.store offset=8 (local.get $temp_next)
                           (local.get $ptr)))
            
            (br $forward_loop)
        ))
        
        (block $backward_loop_out (loop $backward_loop
            ;; ptr-4 == prev->boundary_size
            (local.set $boundary_ptr (i32.sub (local.get $ptr) (i32.const 4)))
            ;; if the previous block isn't free, exit the loop.
            (br_if $backward_loop_out (i32.eqz (i32.and (i32.load (local.get $boundary_ptr))
                                                                  (i32.const 1))))

            ;; temp_size = prev->boundary_size & -2
            (local.set $temp_size (i32.and (i32.load (local.get $boundary_ptr)) (i32.const -2)))

            ;; prev = ptr - temp_size
            (local.set $prev (i32.sub (local.get $ptr) (local.get $temp_size)))
            
            ;; temp_next = ptr->next
            (local.set $temp_next (i32.load offset=4 (local.get $ptr)))

            ;; size += temp_size
            (local.set $size (i32.add (local.get $size) (local.get $temp_size)))

            ;; prev->size = size | 1
            (i32.store (local.get $prev) (i32.or (local.get $size) (i32.const 1)))

            ;; prev->boundary_size = size | 1
            (i32.store (i32.add (local.get $prev)
                                (i32.sub (local.get $size)
                                         (i32.const 4)))
                       (i32.or (local.get $size) (i32.const 1)))


            ;; prev->next = temp_next
            (i32.store offset=4 (local.get $prev) (local.get $temp_next))

            ;; if temp_next != null, temp_next->prev = prev
            (if (i32.ne (i32.const 0) (local.get $temp_next))
                (i32.store offset=8 (local.get $temp_next)
                           (local.get $prev)))

            ;; ptr = prev
            (local.set $ptr (local.get $prev))

            (br $backward_loop)
        ))
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
    ;;  way that it would be more efficient when we can just expand/split the current block.
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
    (table 1 funcref)

    ;;; Re-export memory to work with the WASI ABI
    (export "memory" (memory 0))
    (export "table" (table 0))

    ;; === Reference Counting === ;;

    ;;; type dropper = fn(ptr: *const ());
    (type $dropper (func (param i32)))

    ;;; fn rc_create<T>(val: Box<T>, val_len: u32, drop_in_place: tblidx<fn(&mut T)>) -> Rc<T>;
    ;;; Create an Rc<T> from a boxed `val`. `val_len` MUST equal `sizeof(T)`.
    ;;; When the refcount is zero, the value is dropped using `drop_in_place`.
    ;;; `drop_in_place` MUST NOT deallocate the backing memory of T, just
    ;;; deallocate any owned resources contained inside it.
    (func $rc_create (param $val i32) (param $val_len i32) (param $drop_in_place i32) (result i32)
        (local $rc_ptr i32)
        ;; save space for refcount and tblidx at front
        (local.set $rc_ptr
            (call $realloc
                (local.get $val)
                (i32.add (local.get $val_len) (i32.const 8))))
        (call $memcpy
            (i32.add (local.get $rc_ptr) (i32.const 8))
            (local.get $rc_ptr)
            (local.get $val_len))
        (drop)
        ;; rc_ptr->rc = 1;
        (i32.store (local.get $rc_ptr) (i32.const 1))
        ;; rc_ptr->fnptr = drop_in_place;
        (i32.store offset=4 (local.get $rc_ptr) (local.get $drop_in_place))
        (local.get $rc_ptr))
    
    ;;; fn rc_get<T>(rc_ptr: Rc<T>) -> &T
    ;;; Get the value stored in an Rc<T>. It is NOT SAFE to mutate this value.
    (func $rc_get (param $rc_ptr i32) (result i32)
        (i32.add (local.get $rc_ptr) (i32.const 8)))
    
    ;;; fn rc_acq<T>(rc_ptr: Rc<T>)
    ;;; Acquire (clone) an Rc<T>. This will increase the refcount by 1.
    (func $rc_acq (param $rc_ptr i32)
        (i32.store (local.get $rc_ptr)
                   (i32.add (i32.const 1)
                            (i32.load (local.get $rc_ptr)))))
    
    ;;; fn rc_rel<T>(rc_ptr: Rc<T>)
    ;;; Release (drop) an Rc<T>. This will decrease the refcount by 1,
    ;;; and if needed, drop the value.
    (func $rc_rel (param $rc_ptr i32)
        (local $refcount i32)
        (local.set $refcount (i32.load (local.get $rc_ptr)))
        (if (i32.eqz (local.get $refcount)) 
            (block
                (call $print
                    (global.get $data_panic_release_rc_at_0_offset)
                    (global.get $data_panic_release_rc_at_0_size))
                (drop)
                (call $proc_exit (i32.const 1))
                (unreachable)))
        (local.set $refcount (i32.sub (local.get $refcount) (i32.const 1)))
        (i32.store (local.get $rc_ptr) (local.get $refcount))
        (if (i32.eqz (local.get $refcount))
            (block
                (call_indirect (type $dropper)
                               (i32.add (local.get $rc_ptr) (i32.const 8))
                               (i32.load offset=4 (local.get $rc_ptr)))
                (call $dealloc (local.get $rc_ptr))
            ))
        )

    ;; === Helper Functions === ;;

    ;; === Memory Helpers === ;;

    ;;; fn __memcmp_fast(s1: *const u8, s2: *const u8, n: usize, n_fast: usize, n_slow: usize) -> i32;
    ;;; memcmp fast path (uses SIMD)
    ;;; Do not use directly, just use memcmp.
    (func $__memcmp_fast (param $s1 i32) (param $s2 i32) (param $n i32)
                       (param $n_fast i32) (param $n_slow i32) (result i32)
        (local $v v128) (local $tmp_ptr i32)
        (block $loop_out (loop $loop
            (br_if $loop_out (i32.eqz (local.get $n_fast)))

            (local.set $v (i8x16.sub (v128.load (local.get $s1))
                                     (v128.load (local.get $s2))))

            (if (v128.any_true (local.get $v))
                (block
                    ;; I don't know of a better way to do this :/
                    (v128.store (global.get $data_garbage_v128_offset) (local.get $v))
                    (local.set $tmp_ptr (global.get $data_garbage_v128_offset))
                    (block $slow_loop_out (loop $slow_loop
                        (br_if $slow_loop_out (i32.ne (i32.const 0) (i32.load8_s (local.get $tmp_ptr))))
                        (local.set $tmp_ptr (i32.add (local.get $tmp_ptr) (i32.const 1)))
                        (br $slow_loop)
                    ))
                    (return (i32.load (local.get $tmp_ptr)))
                ))

            (local.set $s1 (i32.add (local.get $s1) (i32.const 16)))
            (local.set $s2 (i32.add (local.get $s2) (i32.const 16)))
            (local.set $n_fast (i32.sub (local.get $n_fast) (i32.const 1)))
            (br $loop)
        ))

        (if (i32.eqz (local.get $n_slow))
            (return (i32.const 0))
            (return (call $__memcmp_slow (local.get $s1) (local.get $s2) (local.get $n_slow))))
        (unreachable)
    )

    ;;; fn __memcmp_slow(s1: *const u8, s2: *const u8, n: usize) -> i32;
    ;;; memcmp slow path (cannot use SIMD as n < 16)
    ;;; Do not use directly, just use memcmp.
    (func $__memcmp_slow (param $s1 i32) (param $s2 i32) (param $n i32) (result i32)
        (local $x i32)

        (block $loop_out (loop $loop
            (br_if $loop_out (i32.eqz (local.get $n)))

            (local.tee $x (i32.sub
                            (i32.load8_s (local.get $s1))
                            (i32.load8_s (local.get $s2))))

            (if (i32.ne (i32.const 0))
                (return (local.get $x)))

            (local.set $s1 (i32.add (local.get $s1) (i32.const 1)))
            (local.set $s2 (i32.add (local.get $s2) (i32.const 1)))
            (local.set $n (i32.sub (local.get $n) (i32.const 1)))
            (br $loop)
        ))

        (i32.const 0)
    )

    ;;; fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32;
    ;;; This function emulates libc's memcmp.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `s1` and `s2` must be valid well-aligned pointers
    ;;; - `s1` and `s2` must point to valid data up to an offset of `n` bytes
    (func $memcmp (param $s1 i32) (param $s2 i32) (param $n i32) (result i32)
        ;; if s1 == s2, then the result is always `0` (equal)
        (if (i32.eq
                (local.get $s1) (local.get $s2))
            (return (i32.const 0)))

        ;; if n >= 16, we can vectorize.
        (if (i32.ge_u (local.get $n) (i32.const 16))
            (return (call $__memcmp_fast (local.get $s1) (local.get $s2) (local.get $n)
                (i32.div_u (local.get $n) (i32.const 16))
                (i32.rem_u (local.get $n) (i32.const 16))))
            (return (call $__memcmp_slow (local.get $s1) (local.get $s2) (local.get $n))))
        (unreachable)
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

    ;; === Numerics === ;;

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

    ;;; fn rem_abs_i32(a: i32, b: i32, signed: bool) -> i32
    (func $rem_abs_i32 (param $a i32) (param $b i32) (param $signed i32) (result i32)
        (select
            (call $i32s_abs (i32.rem_s (local.get $a) (local.get $b)))
            (i32.rem_u (local.get $a) (local.get $b))
            (local.get $signed)))
    
    ;;; fn div_abs_i32(a: i32, b: i32, signed: bool) -> i32
    (func $div_abs_i32 (param $a i32) (param $b i32) (param $signed i32) (result i32)
        (select
            (call $i32s_abs (i32.div_s (local.get $a) (local.get $b)))
            (i32.div_u (local.get $a) (local.get $b))
            (local.get $signed)))

    ;;; fn i32_to_string(num: i32, signed: bool) -> (*mut u8, usize)
    ;;; Converts a 32-bit integer to a string, allocated on the heap by this
    ;;; function. `signed` determines whether or not the integer is interpreted
    ;;; as unsigned or signed.
    (func $i32_to_string (param $num i32) (param $signed i32) (result i32 i32)
        (local $buf i32) (local $buf_new i32)
        (local $ptr i32)
        (local $is_negative i32)
        (local $len i32)

        ;; Allocate a buffer 11 characters long (the text length of
        ;; `-2**31`, the minimum value of an `i32`)
        ;; This is big enough for -2**31 = -2147483648 (min i32)
        ;; and for                 2**32 =  4294967296 (max u32)
        (local.set $buf (call $malloc
            (i32.const 11)))

        ;; Set the pointer to the end of the buffer
        (local.set $ptr
            (i32.add (local.get $buf) (i32.const 10)))

        ;; Test whether or not this number is negative
        (local.set $is_negative
            (i32.lt_s (local.get $num) (i32.const 0)))

        (block $loop_out (loop $loop
            ;; Set the index of the current pointer to the digit as ASCII
            ;; `*ptr = '0' as u8 + abs(num % 10) as u8`
            (i32.store8
                (local.get $ptr)
                (i32.add
                    (i32.const 0x30) ;; 0x30 - ASCII value of `0`
                    (call $rem_abs_i32 (local.get $num) (i32.const 10) (local.get $signed))))
            
            ;; Decrement the pointer
            (local.set $ptr
                (i32.sub (local.get $ptr) (i32.const 1)))

            ;; Divide the number by 10 and compute the absolute value
            ;; `num = abs(num / 10)`
            (local.set $num
                (call $div_abs_i32 (local.get $num) (i32.const 10) (local.get $signed)))

            ;; Break if the number is 0
            (br_if $loop_out
                (i32.eqz (local.get $num)))
            
            ;; Continue loop
            (br $loop)
        ))

        ;; Add a negative sign if the number was negative
        (if (i32.and (local.get $is_negative) (local.get $signed))
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

        ;; Allocate buffer to the right length
        (local.set $buf_new (call $malloc
            (local.get $len)))
        
        ;; Copy data to new buffer
        (call $memcpy
            (local.get $buf_new)
            (local.get $ptr)
            (local.get $len))
        (drop)

        ;; Deallocate old buffer, from start
        (call $dealloc (local.get $buf))

        ;; Return the new buffer and length
        (local.get $len)
        (local.get $buf_new)
    )

    ;; === printf & co. === ;;

    ;;; fn print(str: *const u8, len: u32) -> errno;
    ;;; Print a string to stdout.
    ;;;
    ;;; # Safety
    ;;;
    ;;; - `str` must be a valid, well-aligned pointer to a valid UTF-8 string
    ;;; - `str` must point to valid data up to an offset of `n` bytes
    (func $print (param $str i32) (param $len i32) (result i32)
        ;; static_ciovec->str = str
        (i32.store
            (global.get $data_static_ciovec_offset)
            (local.get $str))

        ;; static_ciovec->len = len
        (i32.store offset=4
            (global.get $data_static_ciovec_offset)
            (local.get $len))

        ;; Write the data to stdout
        (call $fd_write
            (i32.const 1)
            ;; This is a static ciovec used only for this function and other functions that just need a single ciovec.
            ;; While yes, static mutable things are generally bad, we're working on one thread. Guaranteed.
            ;;   (Like, we don't even  have atomics *enabled* guaranteed.) So it doesn't matter.
            ;; And we don't care if other functions clobber our data afterwards, as we only need
            ;;  this for the duration of this fd_write call.
            (global.get $data_static_ciovec_offset)
            (i32.const 1)
            ;; We use the `garbage_u32` for n_written as we need a valid pointer, but we don't care about the data.
            ;; and garbage_u32 is just for this purpose: useless data that's only written, never read.
            (global.get $data_garbage_u32_offset))
    )

    ;;; fn snprintf(
    ;;;     buf: *mut u8, buf_len: isize,
    ;;;     fmt: *const u8, fmt_len: isize,
    ;;;     args: *const u8) -> isize;
    ;;; Print a formatted string to the provided buffer.
    ;;; If the string could be fully printed, the number of characters printed is returned.
    ;;; Otherwise, `-new_size` is returned where `new_size` is how large the buffer needs to be to successfully print.
    ;;;
    ;;; `args` is a pointer to an argument list, whose format is explained below.
    ;;; `args` may be null (0) IF AND ONLY IF the format string provided does not contain ANY specifiers (excluding `%%`).
    ;;;
    ;;; # Supported Format Specifiers
    ;;; 
    ;;; - `%s`: string
    ;;; - `%d`: i32
    ;;; - `%%`: just prints `%`.
    ;;;
    ;;; # Argument List
    ;;;
    ;;; The following list contains inline struct definitions for printf-family functions.
    ;;; All padding fields are undefined and may be arbitrary data.
    ;;;
    ;;; - `%s`:
    ;;;     ptr: *mut u8, len: usize
    ;;; - `%d`:
    ;;;     val: i32, padding: u32
    (func $snprintf (param $buf i32) (param $buf_len i32)
                    (param $fmt i32) (param $fmt_len i32)
                    (param $args i32) (result i32)
        (local $fmt_i i32) (local $buf_i i32)
        (local $byte i32) (local $byte_next i32)
        (local $fmt_ptr i32) (local $buf_ptr i32)
        (local $copy_begin i32) (local $copy_len i32)
        (local $arg_i i32)
        (local $arg_ptr i32) (local $str_ptr i32) (local $str_len i32)
        (local $simulate i32)

        (block $loop_out (loop $loop
            (br_if $loop_out (i32.ge_u (local.get $fmt_i) (local.get $fmt_len)))
            (if (i32.ge_u (local.get $buf_i) (local.get $buf_len))
                (local.set $simulate (i32.const 1)))
            
            (local.set $fmt_ptr (i32.add (local.get $fmt) (local.get $fmt_i)))
            (local.set $buf_ptr (i32.add (local.get $buf) (local.get $buf_i)))

            (if (i32.eqz (local.get $copy_len))
                (local.set $copy_begin (local.get $fmt_ptr)))

            (local.set $byte (i32.load8_u (local.get $fmt_ptr)))

            (if (i32.eq (local.get $byte) (i32.const 0x25)) ;; 0x25 - `%`
                (block
                    ;; handle copying a block, if one exists
                    (if (i32.ne (local.get $copy_len) (i32.const 0))
                        (block
                            ;; buf_i + copy_len >= buf_len => can't copy this block
                            (if (i32.ge_u (i32.add (local.get $buf_i) (local.get $copy_len))
                                          (local.get $buf_len))
                                (local.set $simulate (i32.const 1)))
                            
                            (if (i32.eqz (local.get $simulate))
                                (block
                                    (call $memcpy
                                        (local.get $buf_ptr)
                                        (local.get $copy_begin)
                                        (local.get $copy_len))
                                    (drop)))


                            ;; reset copy_len, update buf_ptr and buf_i
                            (local.set $buf_ptr (i32.add (local.get $buf_ptr) (local.get $copy_len)))
                            (local.set $buf_i (i32.add (local.get $buf_i) (local.get $copy_len)))
                            (local.set $copy_len (i32.const 0))
                        ))

                    ;; fmt_i + 1 >= fmt_len => panic, we can't pull another char
                    ;; TODO: panic
                    (if (i32.ge_u (i32.add (local.get $fmt_i) (i32.const 1))
                                  (local.get $fmt_len))
                        (unreachable))
                    
                    (local.set $byte_next (i32.load8_u offset=1 (local.get $fmt_ptr)))

                    (if (i32.eq (local.get $byte_next) (i32.const 0x25)) ;; 0x25 - `%`
                        (block
                            (if (i32.eqz (local.get $simulate))
                                (i32.store8 (local.get $buf_ptr) (i32.const 0x25)))
                            (local.set $fmt_i (i32.add (local.get $fmt_i) (i32.const 2)))
                            (local.set $buf_i (i32.add (local.get $buf_i) (i32.const 1)))
                            (br $loop)))
                    (if (i32.eq (local.get $byte_next) (i32.const 0x73)) ;; 0x73 - `s`
                        (block
                            (local.set $arg_ptr (i32.add (local.get $args) (i32.mul (local.get $arg_i)
                                                                                    ;; 8: sizeof(arg)
                                                                                    (i32.const 8))))
                            (local.set $str_ptr (i32.load (local.get $arg_ptr)))
                            (local.set $str_len (i32.load offset=4 (local.get $arg_ptr)))

                            ;; buf_i + str_len >= buf_len => can't copy this string
                            (if (i32.ge_u (i32.add (local.get $buf_i) (local.get $str_len))
                                          (local.get $buf_len))
                                (local.set $simulate (i32.const 1)))
                            
                            (if (i32.eqz (local.get $simulate))
                                (block
                                    (call $memcpy
                                        (local.get $buf_ptr)
                                        (local.get $str_ptr)
                                        (local.get $str_len))
                                    (drop)))

                            (local.set $arg_i (i32.add (local.get $arg_i) (i32.const 1)))
                            (local.set $fmt_i (i32.add (local.get $fmt_i) (i32.const 2)))
                            (local.set $buf_i (i32.add (local.get $buf_i) (local.get $str_len)))
                            (br $loop)))
                    (if (i32.eq (local.get $byte_next) (i32.const 0x64)) ;; 0x64 - `d`
                        (block
                            (local.set $arg_ptr (i32.add (local.get $args) (i32.mul (local.get $arg_i) (i32.const 8))))

                            (call $i32_to_string (i32.load (local.get $arg_ptr)) (i32.const 1))
                            (local.set $str_ptr)
                            (local.set $str_len)

                            ;; buf_i + str_len >= buf_len => can't copy this string
                            (if (i32.ge_u (i32.add (local.get $buf_i) (local.get $str_len))
                                          (local.get $buf_len))
                                (local.set $simulate (i32.const 1)))

                            (if (i32.eqz (local.get $simulate))
                                (block
                                    (call $memcpy
                                        (local.get $buf_ptr)
                                        (local.get $str_ptr)
                                        (local.get $str_len))
                                    (drop)))
                            
                            (call $dealloc (local.get $str_ptr))

                            (local.set $arg_i (i32.add (local.get $arg_i) (i32.const 1)))
                            (local.set $fmt_i (i32.add (local.get $fmt_i) (i32.const 2)))
                            (local.set $buf_i (i32.add (local.get $buf_i) (local.get $str_len)))
                            (br $loop)))
                    (if (i32.eq (local.get $byte_next) (i32.const 0x75)) ;; 0x75 - `u`
                        (block
                            (local.set $arg_ptr (i32.add (local.get $args) (i32.mul (local.get $arg_i) (i32.const 8))))

                            (call $i32_to_string (i32.load (local.get $arg_ptr)) (i32.const 0))
                            (local.set $str_ptr)
                            (local.set $str_len)

                            ;; buf_i + str_len >= buf_len => can't copy this string
                            (if (i32.ge_u (i32.add (local.get $buf_i) (local.get $str_len))
                                          (local.get $buf_len))
                                (local.set $simulate (i32.const 1)))

                            (if (i32.eqz (local.get $simulate))
                                (block
                                    (call $memcpy
                                        (local.get $buf_ptr)
                                        (local.get $str_ptr)
                                        (local.get $str_len))
                                    (drop)))
                            
                            (call $dealloc (local.get $str_ptr))

                            (local.set $arg_i (i32.add (local.get $arg_i) (i32.const 1)))
                            (local.set $fmt_i (i32.add (local.get $fmt_i) (i32.const 2)))
                            (local.set $buf_i (i32.add (local.get $buf_i) (local.get $str_len)))
                            (br $loop)))

                    ;; TODO: panic, malformed fmt string
                    (unreachable)
                )
                (block
                    (local.set $copy_len (i32.add (local.get $copy_len) (i32.const 1)))
                ))

            (local.set $fmt_i (i32.add (local.get $fmt_i) (i32.const 1)))
            (br $loop)
        ))

        ;; handle copying a block, if one exists
        (if (i32.ne (local.get $copy_len) (i32.const 0))
            (block
                ;; buf_i + copy_len >= buf_len => can't copy this block
                (if (i32.ge_u (i32.add (local.get $buf_i) (local.get $copy_len))
                              (local.get $buf_len))
                    (local.set $simulate (i32.const 1)))
                
                (if (i32.eqz (local.get $simulate))
                    (block
                        (call $memcpy
                            (local.get $buf_ptr)
                            (local.get $copy_begin)
                            (local.get $copy_len))
                        (drop)))

                ;; reset copy_len, update buf_ptr and buf_i
                (local.set $buf_ptr (i32.add (local.get $buf_ptr) (local.get $copy_len)))
                (local.set $buf_i (i32.add (local.get $buf_i) (local.get $copy_len)))
                (local.set $copy_len (i32.const 0))
            ))

        (select
            (i32.sub (i32.const 0)
                     (i32.add (local.get $buf_i) (i32.const 1)))
            (local.get $buf_i)
            (local.get $simulate))
    )

    ;;; fn sprintf(fmt: *mut u8, fmt_len: isize, args: *const u8) -> (*mut u8, isize);
    ;;; Print a formatted string to a buffer allocated by this function.
    ;;; See `snprintf` for more documentation.
    (func $sprintf (param $fmt i32) (param $fmt_len i32) (param $args i32) (result i32 i32)
        (local $buf i32) (local $len i32) (local $res i32)
    
        ;; Estimate a len ≈ fmt_len + 32
        ;; For what I'd consider "most" format strings (excluding, possibly, ones using `%s`), this is fine.
        (local.set $len (i32.add (local.get $fmt_len) (i32.const 32)))

        (local.set $buf (call $malloc (local.get $len)))

        (local.set $res (call $snprintf (local.get $buf) (local.get $len)
                                        (local.get $fmt) (local.get $fmt_len)
                                        (local.get $args)))
        (if (i32.lt_s (local.get $res) (i32.const 0))
            (block
                ;; extend the buffer as needed
                (local.set $len (i32.sub (i32.const 0) (local.get $res)))
                (local.set $buf (call $realloc (local.get $buf) (local.get $len)))
                (local.set $res (call $snprintf (local.get $buf) (local.get $len)
                                                (local.get $fmt) (local.get $fmt_len)
                                                (local.get $args)))
            )
            (block
                ;; large string/buffer optimization: shrink the buffer
                (local.set $buf (call $realloc (local.get $buf) (local.get $res)))
            ))
        (local.get $res)
        (local.get $buf)
    )

    ;;; fn printf(fmt: *mut u8, fmt_len: isize, args: *const u8);
    ;;; Print a formatted string to stdout.
    ;;; See `snprintf` for more documentation.
    (func $printf (param $fmt i32) (param $fmt_len i32) (param $args i32)
        (local $str i32) (local $len i32)
        (call $sprintf (local.get $fmt) (local.get $fmt_len) (local.get $args))
        (local.set $str)
        (local.set $len)

        (call $print (local.get $str) (local.get $len))
        (drop)
        
        (call $dealloc (local.get $str)))

    ;; === Start === ;;

    (func $main (export "_start")
        (local $arg i32) (local $rc i32)

        (local.set $arg
            (call $malloc (i32.const 32)))
        (i32.store (local.get $arg) (global.get $data_str2_offset))
        (i32.store offset=4 (local.get $arg) (global.get $data_str2_size))
        (i32.store offset=8 (local.get $arg) (i32.const 4294967295))
        (i32.store offset=16 (local.get $arg) (i32.const 4294967295))

        (call $printf (global.get $data_str3_offset) (global.get $data_str3_size) (local.get $arg))

        (call $dealloc
            (local.get $arg))

        (local.set $rc (call $malloc (i32.const 4)))
        (i32.store (local.get $rc) (i32.const 12345))
        (local.set $rc (call $rc_create (local.get $rc) (i32.const 4) (global.get $elem_dropper_func)))
        (call $rc_acq (local.get $rc))
        (call $rc_rel (local.get $rc))
        (call $rc_rel (local.get $rc))
    )

    (func $dropper_func (param $ptr i32)
        (local $arg i32)
        (local.set $arg (call $malloc (i32.const 4)))
        (i32.store (local.get $arg) (i32.load (local.get $ptr)))
        (call $printf (global.get $data_str4_offset) (global.get $data_str4_size) (local.get $arg))
        (call $dealloc (local.get $arg))
    )

    ;; === Static Data === ;;

;;DATA BEGIN;;
    ;;; name: panic_double_dealloc
    ;;; size: 0x18
    (global $data_panic_double_dealloc_offset i32 (i32.const 0x00))
    (global $data_panic_double_dealloc_size i32 (i32.const 0x18))
    (data (i32.const 0x00) "panic: double dealloc()\n")

    ;;; name: panic_release_rc_at_0
    ;;; size: 0x35
    (global $data_panic_release_rc_at_0_offset i32 (i32.const 0x18))
    (global $data_panic_release_rc_at_0_size i32 (i32.const 0x35))
    (data (i32.const 0x18) "panic: tried to rc_rel an Rc<T> with a refcount of 0\n")

    ;;; name: str1
    ;;; size: 0x0e
    (global $data_str1_offset i32 (i32.const 0x50))
    (global $data_str1_size i32 (i32.const 0x0e))
    (data (i32.const 0x50) "Hello, world!\n")

    ;;; name: str2
    ;;; size: 0x0f
    (global $data_str2_offset i32 (i32.const 0x60))
    (global $data_str2_size i32 (i32.const 0x0f))
    (data (i32.const 0x60) "This is a test!")

    ;;; name: str3
    ;;; size: 0x17
    (global $data_str3_offset i32 (i32.const 0x70))
    (global $data_str3_size i32 (i32.const 0x17))
    (data (i32.const 0x70) "Hello, world! %s %d %u\n")

    ;;; name: str4
    ;;; size: 0x0b
    (global $data_str4_offset i32 (i32.const 0x88))
    (global $data_str4_size i32 (i32.const 0x0b))
    (data (i32.const 0x88) "dropped %u\n")

    ;;; name: garbage_u32
    ;;; size: 0x04
    (global $data_garbage_u32_offset i32 (i32.const 0x94))
    (global $data_garbage_u32_size i32 (i32.const 0x04))
    (data (i32.const 0x94) "\00\00\00\00")

    ;;; name: static_ciovec
    ;;; size: 0x08
    (global $data_static_ciovec_offset i32 (i32.const 0x98))
    (global $data_static_ciovec_size i32 (i32.const 0x08))
    (data (i32.const 0x98) "\00\00\00\00\00\00\00\00")

    ;;; name: garbage_v128
    ;;; size: 0x10
    (global $data_garbage_v128_offset i32 (i32.const 0xa0))
    (global $data_garbage_v128_size i32 (i32.const 0x10))
    (data (i32.const 0xa0) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")

    (global $data_end i32 (i32.const 0xb0))

    (global $elem_dropper_func i32 (i32.const 0))
    (elem (i32.const 0) $dropper_func)
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