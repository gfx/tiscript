pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod file_io;
pub mod instructions;
pub mod parser;
pub mod type_checker;
pub mod value;
pub mod vm;

use std::sync::atomic::AtomicBool;

static DEBUG: AtomicBool = AtomicBool::new(false);

pub static TAG: &str = "titys";

#[inline]
pub fn is_debug() -> bool {
    DEBUG.load(std::sync::atomic::Ordering::Relaxed)
}

pub fn set_debug(debug: bool) {
    DEBUG.store(debug, std::sync::atomic::Ordering::Relaxed);
}

#[macro_export]
macro_rules! dprintln {
    ($fmt:literal) => {
        if ::titys::is_debug() {
            print!("[{}] ", ::titys::TAG);
            println!($fmt);
        }
    };
    ($fmt:literal, $($args:expr),*) => {
        if ::titys::is_debug() {
            print!("[{}] ", ::titys::TAG);
            println!($fmt, $($args),*);
        }
    };
}
