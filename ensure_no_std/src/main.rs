#![feature(default_alloc_error_handler)]
#![feature(start)]

#![deny(warnings)]

#![no_std]

use core::alloc::Layout;
use core::panic::PanicInfo;
use dyn_context::free_lifetimes;
use dyn_context::state::{SelfState};
use core::mem::replace;
use core::ops::Deref;
#[cfg(not(windows))]
use libc::exit;
use libc_alloc::LibcAlloc;
#[cfg(windows)]
use winapi::shared::minwindef::UINT;
#[cfg(windows)]
use winapi::um::processthreadsapi::ExitProcess;

#[cfg(windows)]
#[link(name="msvcrt")]
extern { }

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[cfg(windows)]
unsafe fn exit(code: UINT) -> ! {
    ExitProcess(code);
    loop { }
}

#[panic_handler]
pub extern fn panic(_info: &PanicInfo) -> ! {
    unsafe { exit(99) }
}

#[no_mangle]
pub fn rust_oom(_layout: Layout) -> ! {
    unsafe { exit(98) }
}


free_lifetimes! {
    struct State1 {
        a: const u8,
        b: 'b ref u16,
        c: 'c mut u32,
    }
}

impl SelfState for State1 { }

fn test_state_1() {
    let mut x = 3;
    let res = State1Builder {
        a: 1,
        b: &2,
        c: &mut x
    }.build_and_then(|state| {
        assert_eq!(state.a(), 1u8);
        assert_eq!(state.b(), &2u16);
        assert_eq!(replace(state.c_mut(), 12), 3u32);
        "res"
    });
    assert_eq!(res, "res");
    assert_eq!(x, 12);
}

free_lifetimes! {
    #[derive(Debug)]
    pub struct Items<ItemType: 'static> {
        items: 'items ref [ItemType],
    }
}

impl<ItemType> Deref for Items<ItemType> {
    type Target = [ItemType];

    fn deref(&self) -> &Self::Target {
        self.items()
    }
}

fn free_lifetimes_with_generics() {
    let items = &[1, 2, 3];
    let sum: u8 = ItemsBuilder {
        items
    }.build_and_then(|items| {
        items.iter().sum()
    });
    assert_eq!(sum, 6);
}

#[start]
pub fn main(_argc: isize, _argv: *const *const u8) -> isize {
    test_state_1();
    free_lifetimes_with_generics();
    0
}
