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

pub use crate::{
    error::{Error, ErrorKind},
    number::JsonNumber,
    string::{JsonString, JsonStringInfo},
    value::{Object, ParseConfig, Value},
};

// pub mod parse;
mod error;
mod number;
mod string;
#[cfg(test)]
mod tests;
mod value;
