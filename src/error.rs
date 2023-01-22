use std::convert::Infallible;
use std::fmt::Display;

/// A JSON parsing error with location information.
#[derive(Debug, PartialEq)]
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

impl<DelegateError> std::error::Error for Error<DelegateError> where
    DelegateError: std::fmt::Debug + Display
{
}

impl<DelegateError> Display for Error<DelegateError>
where
    DelegateError: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error at {}: {}", self.offset, self.kind)
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
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind<DelegateError = Infallible> {
    /// An invalid UTF-8 sequence was encountered.
    Utf8,
    /// An end-of-file was encountered when it wasn't expected.
    UnexpectedEof,
    /// While parsing an object, something was encountered that was not a valid object key.
    ExpectedObjectKey,
    /// While parsing an object, a colon was expected after an object's key.
    ExpectedColon,
    /// A [`Value`][crate::Value] was expected.
    ExpectedValue,
    /// While parsing an object, either the end of an object (`}`) or a comma
    /// was expected.
    ExpectedCommaOrEndOfObject,
    /// While parsing an array, either the end of an array (`]`) or a comma was
    /// expected.
    ExpectedCommaOrEndOfArray,
    /// When parsing an object or an array, a trailing comma was detected. The
    /// JSON specification disallows trailing commas.
    IllegalTrailingComma,
    /// An unexpected character was encountered while parsing a
    /// [`Value`][crate::Value].
    Unexpected(u8),
    /// The source being parsed contained additional non-whitespace data after a
    /// [`Value`][crate::Value] was parsed.
    TrailingNonWhitespace,
    /// A non-string was encountered for an object key. The JSON standard
    /// requires all keys to be strings.
    ObjectKeysMustBeStrings,
    /// An exponent was expected on a floating point number.
    ExpectedExponent,
    /// At least one decimal digit was expected on a floating point number.
    ExpectedDecimalDigit,
    /// At least one integer digit was expected on a floating point number.
    ExpectedDigit,
    /// An invalid hexadecimal character was encountered in a unicode escape
    /// sequence in a string.
    InvalidHexadecimal,
    /// An invalid character was escaped.
    InvalidEscape,
    /// An object is missing its end (`}`).
    UnclosedObject,
    /// An array is missing its end (`]`).
    UnclosedArray,
    /// A string is missing its end (`"`).
    UnclosedString,
    /// A string was expected, but another type was found.
    ExpectedString,
    /// A number was expected, but another type was found.
    ExpectedNumber,
    /// The JSON being parsed has more depth than the parser was configured to
    /// allow.
    RecursionLimitReached,
    /// A value that wasn't an object or array was contained in a JSON payload.
    ///
    /// This error is only returned when the `allow_all_types_at_root`
    /// configuration is set to `false`.
    PayloadsShouldBeObjectOrArray,
    /// An error was returned from a
    /// [`ParseDelegate`](crate::parser::ParseDelegate).
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

impl<DelegateError> std::error::Error for ErrorKind<DelegateError> where
    DelegateError: std::fmt::Debug + Display
{
}

impl<DelegateError> Display for ErrorKind<DelegateError>
where
    DelegateError: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Utf8 => f.write_str("invalid utf-8"),
            ErrorKind::UnexpectedEof => f.write_str("unexpected end of file"),
            ErrorKind::ExpectedObjectKey => f.write_str("expected object key"),
            ErrorKind::ExpectedColon => f.write_str("expected colon (':')"),
            ErrorKind::ExpectedValue => f.write_str("expected value"),
            ErrorKind::ExpectedCommaOrEndOfObject => f.write_str("expected comma or end of object"),
            ErrorKind::ExpectedCommaOrEndOfArray => f.write_str("expected comma or end of array"),
            ErrorKind::IllegalTrailingComma => f.write_str("trailing commas are not allowed"),
            ErrorKind::Unexpected(ch) => write!(f, "unexpected character: '{ch}'"),
            ErrorKind::TrailingNonWhitespace => {
                f.write_str("trailing non-whitespace data was encountered")
            }
            ErrorKind::ObjectKeysMustBeStrings => f.write_str("object keys must be strings"),
            ErrorKind::ExpectedExponent => {
                f.write_str("expected exponent sign or digit for number literal")
            }
            ErrorKind::ExpectedDecimalDigit => {
                f.write_str("expected decimal digit for number literal")
            }
            ErrorKind::ExpectedDigit => f.write_str("expected decimal digit for number literal"),
            ErrorKind::InvalidHexadecimal => {
                f.write_str("invalid hexadecimal in unicode escape sequence")
            }
            ErrorKind::InvalidEscape => f.write_str("invalid escaped character"),
            ErrorKind::UnclosedObject => f.write_str("expected end of object ('}}')"),
            ErrorKind::UnclosedArray => f.write_str("expected end of array (']')"),
            ErrorKind::UnclosedString => f.write_str("expected end of string ('\"')"),
            ErrorKind::ExpectedString => f.write_str("expected a string"),
            ErrorKind::ExpectedNumber => f.write_str("expected a number"),
            ErrorKind::RecursionLimitReached => f.write_str("the recursion limit has been reached"),
            ErrorKind::PayloadsShouldBeObjectOrArray => {
                f.write_str("JSON only allows objects or arrays at the root")
            }
            ErrorKind::ErrorFromDelegate(err) => write!(f, "error from delegate: {err}"),
        }
    }
}
