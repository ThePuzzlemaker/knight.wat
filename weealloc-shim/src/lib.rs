#![no_std]

use core::{
    alloc::{GlobalAlloc, Layout},
    panic::PanicInfo,
};

use wee_alloc::WeeAlloc;

#[no_mangle]
pub static ALLOC: wee_alloc::WeeAlloc = WeeAlloc::INIT;

/// Allocate a block of memory.
///
/// # Safety
///
/// - `align` must not be zero,
/// - `align` must be a power of two
/// - `size`, when rounded up to the nearest multiple of `align`, must not
///   overflow (i.e., the rounded value must be less than or equal to
///   `usize::MAX`),
/// - `size`, when rounded up to the nearest multiple of `align`, must not
///   be zero,
/// - the allocated block of memory may or may not be initialized
///
/// # Errors
///
/// A null pointer indicates that memory could not be allocated.
#[no_mangle]
pub unsafe extern "C" fn malloc(size: usize, align: usize) -> *mut u8 {
    ALLOC.alloc(Layout::from_size_align_unchecked(size, align))
}

/// Deallocate a block of memory.
///
/// # Safety
///
/// - `align` must not be zero,
/// - `align` must be a power of two
/// - `size`, when rounded up to the nearest multiple of `align`, must not
///   overflow (i.e., the rounded value must be less than or equal to
///   `usize::MAX`),
/// - `size`, when rounded up to the nearest multiple of `align`, must not
///   be zero,
/// - `ptr` must be a pointer to a block of memory currently allocated by
///   weealloc
/// - `align` and `size` must be the same values as what was used to allocate
///   the block of memory
#[no_mangle]
pub unsafe extern "C" fn dealloc(ptr: *mut u8, size: usize, align: usize) {
    ALLOC.dealloc(ptr, Layout::from_size_align_unchecked(size, align))
}

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}
