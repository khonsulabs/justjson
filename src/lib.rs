#![doc = include_str!("../README.md")]
#![warn(
    clippy::cargo,
    missing_docs,
    clippy::pedantic,
    future_incompatible,
    rust_2018_idioms
)]
#![allow(
    clippy::option_if_let_else,
    clippy::module_name_repetitions,
    clippy::missing_errors_doc
)]
#![deny(unsafe_code)]
#![no_std]
#![cfg_attr(any(docsrs, feature = "nightly"), feature(doc_auto_cfg))]

#[cfg(any(feature = "std", test))]
extern crate std;

#[cfg(feature = "alloc")]
extern crate alloc;

pub use crate::anystr::AnyStr;
pub use crate::error::{Error, ErrorKind};
pub use crate::number::JsonNumber;
pub use crate::string::{JsonString, JsonStringInfo};
#[cfg(feature = "alloc")]
pub use crate::value::{Object, Value};

mod anystr;
/// A JSON DOM representation with minimal processing.
pub mod doc;
mod error;
mod number;
/// A low-level event-driven JSON parser.
pub mod parser;
mod string;
#[cfg(all(test, feature = "alloc"))]
mod tests;
#[cfg(feature = "alloc")]
mod value;
