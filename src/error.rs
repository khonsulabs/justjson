use std::{io, str::Utf8Error};

/// A JSON parsing error with location information.
#[derive(thiserror::Error, Debug, PartialEq)]
#[error("error at {offset}: {kind}")]
pub struct Error {
    pub(crate) offset: usize,
    pub(crate) kind: ErrorKind,
}

impl From<Utf8Error> for Error {
    fn from(value: Utf8Error) -> Self {
        Self {
            offset: value.valid_up_to(),
            kind: ErrorKind::from(value),
        }
    }
}

impl Error {
    /// Returns the kind of the error.
    #[must_use]
    pub const fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// Returns the offset of the error, which is the byte position nearest to
    /// where the error occurred.
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }
}

/// An error from parsing JSON.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum ErrorKind {
    /// An error occurred while performing io.
    #[error("io error: {0}")]
    Io(io::Error),
    /// An invalid UTF-8 sequence was encountered.
    #[error("invalid utf-8: {0}")]
    Utf8(#[from] std::str::Utf8Error),
    /// An end-of-file was encountered when it wasn't expected.
    #[error("unexpected end of file")]
    UnexpectedEof,
    // #[error("the maximum_string_length has been exceeded")]
    // TemporaryBufferCapacityExceeded,
    /// While parsing an object, something was encountered that was not a valid object key.
    #[error("expected object key")]
    ExpectedObjectKey,
    /// While parsing an object, a colon was expected after an object's key.
    #[error("expected colon (':')")]
    ExpectedColon,
    /// A [`Value`][crate::Value] was expected.
    #[error("expected value")]
    ExpectedValue,
    /// While parsing an object, either the end of an object (`}`) or a comma
    /// was expected.
    #[error("expected comma or end of object")]
    ExpectedCommaOrEndOfObject,
    /// While parsing an array, either the end of an array (`]`) or a comma was
    /// expected.
    #[error("expected comma or end of array")]
    ExpectedCommaOrEndOfArray,
    /// When parsing an object or an array, a trailing comma was detected. The
    /// JSON specification disallows trailing commas.
    #[error("trailing commas are not allowed")]
    IllegalTrailingComma,
    /// An unexpected character was encountered while parsing a
    /// [`Value`][crate::Value].
    #[error("unexpected character: '{0}'")]
    Unexpected(u8),
    /// The source being parsed contained additional non-whitespace data after a
    /// [`Value`][crate::Value] was parsed.
    #[error("trailing non-whitespace data was encountered")]
    TrailingNonWhitespace,
    /// A non-string was encountered for an object key. The JSON standard
    /// requires all keys to be strings.
    #[error("object keys must be strings")]
    ObjectKeysMustBeStrings,
    /// An exponent was expected on a floating point number.
    #[error("expected exponent sign or digit for number literal")]
    ExpectedExponent,
    /// At least one decimal digit was expected on a floating point number.
    #[error("expected decimal digit for number literal")]
    ExpectedDecimalDigit,
    /// At least one integer digit was expected on a floating point number.
    #[error("expected decimal digit for number literal")]
    ExpectedDigit,
    /// An invalid hexadecimal character was encountered in a unicode escape
    /// sequence in a string.
    #[error("invalid hexadecimal in unicode escape sequence")]
    InvalidHexadecimal,
    /// An invalid character was escaped.
    #[error("invalid escaped character")]
    InvalidEscape,
    /// An object is missing its end (`}`).
    #[error("expected end of object ('}}')")]
    UnclosedObject,
    /// An array is missing its end (`]`).
    #[error("expected end of array (']')")]
    UnclosedArray,
    /// A string is missing its end (`"`).
    #[error("expected end of string ('\"')")]
    UnclosedString,
    /// A string was expected, but another type was found.
    #[error("expected a string")]
    ExpectedString,
    /// A number was expected, but another type was found.
    #[error("expected a number")]
    ExpectedNumber,
    /// The JSON being parsed has more depth than the parser was configured to
    /// allow.
    #[error("the recursion limit has been reached")]
    RecursionLimitReached,
    /// A value that wasn't an object or array was contained in a JSON payload.
    ///
    /// This error is only returned when the `allow_all_types_at_root`
    /// configuration is set to `false`.
    #[error("the recursion limit has been reached")]
    PayloadsShouldBeObjectOrArray,
}

impl PartialEq for ErrorKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(l0), Self::Io(r0)) => l0.kind() == r0.kind(),
            (Self::Utf8(l0), Self::Utf8(r0)) => l0 == r0,
            (Self::Unexpected(l0), Self::Unexpected(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<io::Error> for ErrorKind {
    fn from(err: io::Error) -> Self {
        if err.kind() == io::ErrorKind::UnexpectedEof {
            Self::UnexpectedEof
        } else {
            Self::Io(err)
        }
    }
}
