use std::convert::Infallible;

/// A JSON parsing error with location information.
#[derive(thiserror::Error, Debug, PartialEq)]
#[error("error at {offset}: {kind}")]
pub struct Error<DelegateError = Infallible> {
    pub(crate) offset: usize,
    pub(crate) kind: ErrorKind<DelegateError>,
}

impl<DelegateError> Error<DelegateError> {
    /// Returns the kind of the error.
    #[must_use]
    pub const fn kind(&self) -> &ErrorKind<DelegateError> {
        &self.kind
    }

    /// Returns the offset of the error, which is the byte position nearest to
    /// where the error occurred.
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }
}

impl Error<Infallible> {
    #[must_use]
    pub(crate) fn into_fallable<DelegateError>(self) -> Error<DelegateError> {
        Error {
            offset: self.offset,
            kind: self.kind.into_fallable(),
        }
    }
}

/// An error from parsing JSON.
#[derive(thiserror::Error, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind<DelegateError = Infallible> {
    /// An invalid UTF-8 sequence was encountered.
    #[error("invalid utf-8")]
    Utf8,
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
    /// An error was returned from a
    /// [`ParseDelegate`](crate::parser::ParseDelegate).
    #[error("error from delegate: {0}")]
    ErrorFromDelegate(DelegateError),
}

impl ErrorKind<Infallible> {
    fn into_fallable<DelegateError>(self) -> ErrorKind<DelegateError> {
        match self {
            ErrorKind::Utf8 => ErrorKind::Utf8,
            ErrorKind::UnexpectedEof => ErrorKind::UnexpectedEof,
            ErrorKind::ExpectedObjectKey => ErrorKind::ExpectedObjectKey,
            ErrorKind::ExpectedColon => ErrorKind::ExpectedColon,
            ErrorKind::ExpectedValue => ErrorKind::ExpectedValue,
            ErrorKind::ExpectedCommaOrEndOfObject => ErrorKind::ExpectedCommaOrEndOfObject,
            ErrorKind::ExpectedCommaOrEndOfArray => ErrorKind::ExpectedCommaOrEndOfArray,
            ErrorKind::IllegalTrailingComma => ErrorKind::IllegalTrailingComma,
            ErrorKind::Unexpected(ch) => ErrorKind::Unexpected(ch),
            ErrorKind::TrailingNonWhitespace => ErrorKind::TrailingNonWhitespace,
            ErrorKind::ObjectKeysMustBeStrings => ErrorKind::ObjectKeysMustBeStrings,
            ErrorKind::ExpectedExponent => ErrorKind::ExpectedExponent,
            ErrorKind::ExpectedDecimalDigit => ErrorKind::ExpectedDecimalDigit,
            ErrorKind::ExpectedDigit => ErrorKind::ExpectedDigit,
            ErrorKind::InvalidHexadecimal => ErrorKind::InvalidHexadecimal,
            ErrorKind::InvalidEscape => ErrorKind::InvalidEscape,
            ErrorKind::UnclosedObject => ErrorKind::UnclosedObject,
            ErrorKind::UnclosedArray => ErrorKind::UnclosedArray,
            ErrorKind::UnclosedString => ErrorKind::UnclosedString,
            ErrorKind::ExpectedString => ErrorKind::ExpectedString,
            ErrorKind::ExpectedNumber => ErrorKind::ExpectedNumber,
            ErrorKind::RecursionLimitReached => ErrorKind::RecursionLimitReached,
            ErrorKind::PayloadsShouldBeObjectOrArray => ErrorKind::PayloadsShouldBeObjectOrArray,
            ErrorKind::ErrorFromDelegate(_) => unreachable!(),
        }
    }
}
