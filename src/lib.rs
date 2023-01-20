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

pub use crate::error::{Error, ErrorKind};
pub use crate::number::JsonNumber;
pub use crate::string::{JsonString, JsonStringInfo};
pub use crate::value::{Object, Value};

/// A JSON DOM representation with minimal processing.
pub mod doc;
mod error;
mod number;
/// A low-level event-driven JSON parser.
pub mod parser;
mod string;
#[cfg(test)]
mod tests;
mod value;
