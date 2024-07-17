pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod de;
pub mod instructions;
pub mod parser;
pub mod ser;
pub mod type_checker;
pub mod util;
pub mod value;
pub mod vm;

pub use crate::de::{from_file, from_str};

use std::sync::atomic::AtomicBool;

static DEBUG: AtomicBool = AtomicBool::new(false);

pub static TAG: &str = "tiscript";

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
        if ::tiscript::is_debug() {
            eprint!("[{}] ", ::tiscript::TAG);
            eprintln!($fmt);
        }
    };
    ($fmt:literal, $($args:expr),*) => {
        if ::tiscript::is_debug() {
            eprint!("[{}] ", ::tiscript::TAG);
            eprintln!($fmt, $($args),*);
        }
    };
}
