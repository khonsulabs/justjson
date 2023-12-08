use core::convert::Infallible;
use core::marker::PhantomData;

use crate::anystr::AnyStr;
use crate::string::{
    merge_surrogate_pair, StringContents, HEX_OFFSET_TABLE, HIGH_SURROGATE_MAX, HIGH_SURROGATE_MIN,
    LOW_SURROGATE_MAX, LOW_SURROGATE_MIN, SAFE_STRING_BYTES, SAFE_STRING_BYTES_VERIFY_UTF8,
};
use crate::{Error, ErrorKind, JsonNumber, JsonString, JsonStringInfo};

/// A JSON Token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    /// The `null` keyword.
    Null,
    /// A boolean literal.
    Bool(bool),
    /// A string literal.
    String(JsonString<'a>),
    /// A numeric literal.
    Number(JsonNumber<'a>),
    /// The beginning of an object (`{`).
    Object,
    /// The end of an object (`}`).
    ObjectEnd,
    /// The beginning of an array (`[`).
    Array,
    /// The end of an array (`]`).
    ArrayEnd,
    /// A colon (`:`), delimiting a key-value pair in an object.
    Colon,
    /// A comma (`,`), delimiting a list of values or key-value pairs.
    Comma,
}

/// The (likely) kind of the next JSON token
///
/// Tries to guess the kind of token present, based on the next character in the token stream
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PeekableTokenKind {
    /// `null` keyword
    ///
    /// Corresponds to [`Token::Null`]
    Null,
    /// `true` keyword
    ///
    /// Corresponds to [`Token::Bool`]
    True,
    /// `false` keyword
    ///
    /// Corresponds to [`Token::Bool`]
    False,
    /// A string literal
    ///
    /// Corresponds to [`Token::String`]
    String,
    /// A numeric literal
    ///
    /// Corresponds to [`Token::Number`]
    Number,
    /// The beginning of an object (`{`)
    ///
    /// Corresponds to [`Token::Object`]
    Object,
    /// The end of an object (`{`)
    ///
    /// Corresponds to [`Token::ObjectEnd`]
    ObjectEnd,
    /// The beginning of an array (`[`)
    ///
    /// Corresponds to [`Token::Array`]
    Array,
    /// The end of an array (`]`)
    ///
    /// Corresponds to [`Token::ArrayEnd`]
    ArrayEnd,
    /// Unable to determine the specific token.
    ///
    /// This kind suggests that the next token is likely malformed, but does not guarantee it.
    Unrecognized,
    /// A colon (`:`), delimiting a key-value pair in an object.
    ///
    /// Corresponds to [`Token::Colon`]
    Colon,
    /// A comma (`,`), delimiting a list of values or key-value pairs.
    ///
    /// Corresponds to [`Token::Comma`]
    Comma,
}

/// A JSON tokenizer, which converts JSON source to a series of [`Token`]s.
pub struct Tokenizer<'a, const GUARANTEED_UTF8: bool> {
    source: ByteIterator<'a>,
}

impl<'a> Tokenizer<'a, true> {
    /// Returns a new tokenizer for the JSON `source` provided.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    #[must_use]
    pub fn for_json(source: &'a str) -> Self {
        Self::new(source.as_bytes())
    }
}

impl<'a> Tokenizer<'a, false> {
    /// Returns a new tokenizer for the JSON `source` bytes provided.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    #[must_use]
    pub fn for_json_bytes(source: &'a [u8]) -> Self {
        Self::new(source)
    }
}

impl<'a, const GUARANTEED_UTF8: bool> Tokenizer<'a, GUARANTEED_UTF8> {
    #[must_use]
    #[inline]
    fn new(source: &'a [u8]) -> Self {
        Self {
            source: ByteIterator::new(source),
        }
    }

    /// Returns the next token, or an [`ErrorKind::UnexpectedEof`].
    ///
    /// This is functionally identical to this type's [`Iterator`]
    /// implementation, except that this function returns an error instead of
    /// None when no more tokens remain.
    #[inline]
    pub fn next_or_eof(&mut self) -> Result<Token<'a>, Error> {
        let (offset, byte) = self.source.read_non_ws().map_err(Error::into_fallable)?;
        self.read_peek(offset, byte)
    }

    #[inline]
    fn read_peek(&mut self, offset: usize, first: &'a u8) -> Result<Token<'a>, Error> {
        match first {
            b'{' => Ok(Token::Object),
            b'}' => Ok(Token::ObjectEnd),
            b'[' => Ok(Token::Array),
            b']' => Ok(Token::ArrayEnd),
            b',' => Ok(Token::Comma),
            b':' => Ok(Token::Colon),
            b'"' => self.read_string_from_source(offset).map(Token::String),
            b'-' => self
                .read_number_from_source(InitialNumberState::Sign, offset)
                .map(Token::Number),
            b'0' => self
                .read_number_from_source(InitialNumberState::Zero, offset)
                .map(Token::Number),
            b'1'..=b'9' => self
                .read_number_from_source(InitialNumberState::Digit, offset)
                .map(Token::Number),
            b't' => self
                .read_literal_from_source(b"rue")
                .map(|()| Token::Bool(true)),
            b'f' => self
                .read_literal_from_source(b"alse")
                .map(|()| Token::Bool(false)),
            b'n' => self.read_literal_from_source(b"ull").map(|()| Token::Null),
            _ => Err(Error {
                offset,
                kind: ErrorKind::Unexpected(*first),
            }),
        }
    }

    #[inline]
    #[allow(unsafe_code)]
    fn read_string_from_source(&mut self, start: usize) -> Result<JsonString<'a>, Error> {
        let safe_strings = if GUARANTEED_UTF8 {
            SAFE_STRING_BYTES
        } else {
            SAFE_STRING_BYTES_VERIFY_UTF8
        };
        let mut string_info = JsonStringInfo::NONE;

        loop {
            let (offset, byte) = self.source.next().ok_or(Error {
                offset: self.source.offset,
                kind: ErrorKind::UnclosedString,
            })?;
            if safe_strings[usize::from(*byte)] {
                string_info.add_bytes(1);
            } else {
                match byte {
                    b'"' => {
                        break Ok(JsonString {
                            source: StringContents::Json(
                                // SAFETY: the UTF8 has been manually verified by the parser.
                                AnyStr::Borrowed(unsafe {
                                    core::str::from_utf8_unchecked(
                                        &self.source.bytes[start + 1..offset],
                                    )
                                }),
                            ),
                            info: string_info,
                        });
                    }
                    b'\\' => self.read_string_escape(&mut string_info)?,
                    128..=255 => {
                        // Manual UTF-8 validation
                        let utf8_start = offset;
                        while let Some(byte) = self.source.peek() {
                            if byte < &128 {
                                break;
                            }

                            self.source.next();
                        }

                        let unicode_end = self.source.offset;
                        string_info.add_bytes(unicode_end - utf8_start);
                        if core::str::from_utf8(&self.source.bytes[utf8_start..unicode_end])
                            .is_err()
                        {
                            // The offset on this is incorrect.
                            return Err(Error {
                                offset,
                                kind: ErrorKind::Utf8,
                            });
                        }
                    }
                    0..=31 => {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::Unexpected(*byte),
                        })
                    }
                    b' '..=127 => {
                        unreachable!("safe_strings filters these values")
                    }
                }
            }
        }
    }

    #[allow(unsafe_code)]
    #[inline]
    fn read_string_escape(&mut self, string_info: &mut JsonStringInfo) -> Result<(), Error> {
        match self.source.read()? {
            (_, b'"' | b'\\' | b'/' | b'b' | b'f' | b'r' | b'n' | b't') => {
                string_info.add_bytes_from_escape(1);
            }
            (offset, b'u') => {
                // 4 hexadecimal digits.
                let mut decoded = 0_u32;
                for _ in 0..4 {
                    let (offset, digit) = self.source.read()?;
                    let nibble = HEX_OFFSET_TABLE[usize::from(*digit)];
                    if nibble == u8::MAX {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::InvalidHexadecimal,
                        });
                    }
                    decoded = decoded << 4 | u32::from(nibble);
                }

                let ch = if let Some(ch) = char::from_u32(decoded) {
                    ch
                } else {
                    // We either have an invalid codepoint or a partial
                    // surrogate.
                    let mut decoded_is_surrogate_pair = false;
                    if (HIGH_SURROGATE_MIN..=HIGH_SURROGATE_MAX).contains(&decoded) {
                        // We have a potential surrogate pair. Try to read another \u escape code
                        if self.source.read()?.1 == &b'\\' && self.source.read()?.1 == &b'u' {
                            let mut second_codepoint = 0;
                            for _ in 0..4 {
                                let (offset, digit) = self.source.read()?;
                                let nibble = HEX_OFFSET_TABLE[usize::from(*digit)];
                                if nibble == u8::MAX {
                                    return Err(Error {
                                        offset,
                                        kind: ErrorKind::InvalidHexadecimal,
                                    });
                                }
                                second_codepoint = second_codepoint << 4 | u32::from(nibble);
                            }
                            if (LOW_SURROGATE_MIN..=LOW_SURROGATE_MAX).contains(&second_codepoint) {
                                // We have a valid surrogate pair
                                decoded = merge_surrogate_pair(decoded, second_codepoint);
                                decoded_is_surrogate_pair = true;
                            }
                        }
                    }

                    if decoded_is_surrogate_pair {
                        // SAFETY: we have manually marged the surrogate pair
                        // into a single valid codepoint, and this cannot fail.
                        unsafe { char::from_u32_unchecked(decoded) }
                    } else {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::Utf8,
                        });
                    }
                };

                string_info.add_bytes_from_escape(ch.len_utf8());
            }
            (offset, _) => {
                return Err(Error {
                    offset,
                    kind: ErrorKind::InvalidEscape,
                })
            }
        }
        Ok(())
    }

    #[allow(unsafe_code)]
    #[inline]
    fn read_number_from_source(
        &mut self,
        initial_state: InitialNumberState,
        start: usize,
    ) -> Result<JsonNumber<'a>, Error> {
        // Numbers are the "hardest" in that we have to peek the digits since
        // there is no terminal character. Every other type in JSON has a way to
        // know when the type ends.

        // First, if the number began with a sign, we must read an integer
        // digit. The JSON spec disallows numbers with leading 0s. If the first
        // digit is a 0, it must be a decimal with a 0 integer value.
        if initial_state != InitialNumberState::Zero {
            let mut read_integer_digit = initial_state == InitialNumberState::Digit;
            while let Some(byte) = self.source.peek() {
                match byte {
                    b'0' if !read_integer_digit => {
                        // 0 after a sign still counts as the complete Integer
                        // part.
                        self.source.read()?;
                        read_integer_digit = true;
                        break;
                    }
                    b'0'..=b'9' => {
                        self.source.read()?;
                        read_integer_digit = true;
                    }
                    _ => break,
                }
            }

            if !read_integer_digit {
                return Err(Error {
                    offset: self.source.offset,
                    kind: ErrorKind::ExpectedDigit,
                });
            }
        }

        // If the next character is a period, this is a floating point literal.
        if let Some(b'.') = self.source.peek() {
            self.source.next();

            // Read one or more decimal digits
            let mut read_decimal_digit = false;
            while let Some(byte) = self.source.peek() {
                if byte.is_ascii_digit() {
                    self.source.next();
                    read_decimal_digit = true;
                } else {
                    break;
                }
            }

            if !read_decimal_digit {
                return Err(Error {
                    offset: self.source.offset,
                    kind: ErrorKind::ExpectedDecimalDigit,
                });
            }
        }

        // Next, we might have an exponent
        if let Some(b'e' | b'E') = self.source.peek() {
            self.source.next();

            // Next, we might have a sign
            if let Some(b'-' | b'+') = self.source.peek() {
                self.source.next();
            }

            // Read one or more exponent digits
            let mut read_exponent_digit = false;
            while let Some(byte) = self.source.peek() {
                if byte.is_ascii_digit() {
                    self.source.next();
                    read_exponent_digit = true;
                } else {
                    break;
                }
            }

            if !read_exponent_digit {
                return Err(Error {
                    offset: self.source.offset,
                    kind: ErrorKind::ExpectedExponent,
                });
            }
        }

        Ok(JsonNumber {
            // SAFETY: To reach this point, we can only have read ascii
            // characters.
            source: AnyStr::Borrowed(unsafe {
                core::str::from_utf8_unchecked(&self.source.bytes[start..self.source.offset])
            }),
        })
    }

    #[inline]
    fn read_literal_from_source(&mut self, remaining_bytes: &[u8]) -> Result<(), Error> {
        for expected in remaining_bytes {
            let (offset, received) = self.source.next().ok_or(Error {
                offset: self.source.offset,
                kind: ErrorKind::UnexpectedEof,
            })?;

            if received != expected {
                return Err(Error {
                    offset,
                    kind: ErrorKind::Unexpected(*received),
                });
            }
        }

        Ok(())
    }

    /// Peeks at the next non-whitespace character, and returns a what kind of
    /// token the next token will be if the input is valid JSON syntax.
    ///
    /// If the source input has no remaining non-whitespace characters, this
    /// function returns `None`.
    #[inline]
    pub fn peek(&mut self) -> Option<PeekableTokenKind> {
        self.source.skip_ws();

        let token = self.source.peek()?;

        let kind = match token {
            b'{' => PeekableTokenKind::Object,
            b'}' => PeekableTokenKind::ObjectEnd,
            b'[' => PeekableTokenKind::Array,
            b']' => PeekableTokenKind::ArrayEnd,
            b',' => PeekableTokenKind::Comma,
            b':' => PeekableTokenKind::Colon,
            b'"' => PeekableTokenKind::String,
            b'-' | b'0'..=b'9' => PeekableTokenKind::Number,
            b't' => PeekableTokenKind::True,
            b'f' => PeekableTokenKind::False,
            b'n' => PeekableTokenKind::Null,
            _ => PeekableTokenKind::Unrecognized,
        };

        Some(kind)
    }

    /// Returns the current byte offset of the tokenizer.
    #[inline]
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.source.offset
    }
}

/// Parses JSON, driven by a [`ParseDelegate`].
///
/// This is a low-level type. In general, users will want to use either the
/// [`Document`](crate::doc::Document) or [`Value`](crate::Value) types instead of
/// directly interacting with the parser.
///
/// This type uses a constant to track whether UTF-8 needs to be manually
/// verified or not. This allows the compiler to optimize the `&[u8]` and `&str`
/// parsing methods independently.
pub struct Parser<'a, const GUARANTEED_UTF8: bool> {
    tokenizer: Tokenizer<'a, GUARANTEED_UTF8>,
}

impl<'a> Parser<'a, false> {
    /// Parses a JSON payload, invoking functions on `delegate` as the payload
    /// is parsed.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn parse_json_bytes<D>(value: &'a [u8], delegate: D) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        Self::parse_json_bytes_with_config(value, ParseConfig::default(), delegate)
    }

    /// Parses a JSON payload, invoking functions on `delegate` as the payload
    /// is parsed. The parser honors the settings from `config`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn parse_json_bytes_with_config<D>(
        value: &'a [u8],
        config: ParseConfig,
        delegate: D,
    ) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        Self::parse_bytes(value, config, delegate)
    }

    /// Validates that `json` contains valid JSON and returns the kind of data
    /// the payload contained.
    pub fn validate_json_bytes(json: &'a [u8]) -> Result<JsonKind, Error> {
        Self::validate_json_bytes_with_config(json, ParseConfig::default())
    }

    /// Validates that `json` contains valid JSON, using the settings from
    /// `config`, and returns the kind of data the payload contained.
    pub fn validate_json_bytes_with_config(
        json: &'a [u8],
        config: ParseConfig,
    ) -> Result<JsonKind, Error> {
        Self::parse_bytes(json, config, ())
    }
}

impl<'a, const GUARANTEED_UTF8: bool> Iterator for Tokenizer<'a, GUARANTEED_UTF8> {
    type Item = Result<Token<'a>, Error>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let (offset, byte) = self.source.next_non_ws()?;
        Some(self.read_peek(offset, byte))
    }
}

impl<'a> Parser<'a, true> {
    /// Parses a JSON payload, invoking functions on `delegate` as the payload
    /// is parsed.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn parse_json<D>(value: &'a str, delegate: D) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        Self::parse_json_with_config(value, ParseConfig::default(), delegate)
    }

    /// Parses a JSON payload, invoking functions on `delegate` as the payload
    /// is parsed. The parser honors the settings from `config`.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn parse_json_with_config<D>(
        value: &'a str,
        config: ParseConfig,
        delegate: D,
    ) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        Self::parse_bytes(value.as_bytes(), config, delegate)
    }

    /// Validates that `json` contains valid JSON and returns the kind of data
    /// the payload contained.
    pub fn validate_json(json: &'a str) -> Result<JsonKind, Error> {
        Self::validate_json_with_config(json, ParseConfig::default())
    }

    /// Validates that `json` contains valid JSON, using the settings from
    /// `config`, and returns the kind of data the payload contained.
    pub fn validate_json_with_config(
        json: &'a str,
        config: ParseConfig,
    ) -> Result<JsonKind, Error> {
        Self::parse_json_with_config(json, config, ())
    }
}

impl<'a, const GUARANTEED_UTF8: bool> Parser<'a, GUARANTEED_UTF8> {
    fn parse_bytes<D>(
        source: &'a [u8],
        config: ParseConfig,
        delegate: D,
    ) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        let mut state = ParseState::new(config, delegate);
        let mut parser = Self {
            tokenizer: Tokenizer::new(source),
        };
        let value = parser.read_from_source(&mut state)?;

        if !state.config.allow_all_types_at_root
            && !matches!(
                state.delegate.kind_of(&value),
                JsonKind::Object | JsonKind::Array
            )
        {
            return Err(Error {
                offset: 0,
                kind: ErrorKind::PayloadsShouldBeObjectOrArray,
            });
        }

        match parser.tokenizer.source.next_non_ws() {
            None => Ok(value),
            Some((offset, _)) => Err(Error {
                offset,
                kind: ErrorKind::TrailingNonWhitespace,
            }),
        }
    }

    #[inline]
    fn read_from_source<D>(
        &mut self,
        state: &mut ParseState<'a, D>,
    ) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        self.read_tokens(state)
    }

    #[inline]
    fn read_tokens<D>(&mut self, state: &mut ParseState<'a, D>) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        let offset = self.tokenizer.offset();
        let token = self.tokenizer.next_or_eof().map_err(Error::into_fallable)?;

        let unexpected = |value: u8| {
            Err(Error {
                offset,
                kind: ErrorKind::Unexpected(value),
            })
        };

        let into_error = |error: D::Error| Error {
            offset,
            kind: ErrorKind::ErrorFromDelegate(error),
        };

        match token {
            Token::Null => state.delegate.null().map_err(into_error),
            Token::Object => {
                state.begin_nest().map_err(|kind| Error { offset, kind })?;

                self.read_object(state).map_err(|mut error| {
                    if matches!(error.kind, ErrorKind::UnexpectedEof) {
                        error.kind = ErrorKind::UnclosedObject;
                    }

                    error
                })
            }
            Token::Array => {
                state.begin_nest().map_err(|kind| Error { offset, kind })?;

                self.read_array(state).map_err(|mut error| {
                    if matches!(error.kind, ErrorKind::UnexpectedEof) {
                        error.kind = ErrorKind::UnclosedArray;
                    }

                    error
                })
            }
            Token::ObjectEnd => unexpected(b'}'),
            Token::ArrayEnd => unexpected(b']'),
            Token::Colon => unexpected(b':'),
            Token::Comma => unexpected(b','),
            Token::Bool(value) => state.delegate.boolean(value).map_err(into_error),
            Token::String(value) => state.delegate.string(value).map_err(into_error),
            Token::Number(value) => state.delegate.number(value).map_err(into_error),
        }
    }

    #[inline]
    fn read_object<D>(&mut self, state: &mut ParseState<'a, D>) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        let offset = self.tokenizer.offset();
        let mut object = state.delegate.begin_object().map_err(|kind| Error {
            offset,
            kind: ErrorKind::ErrorFromDelegate(kind),
        })?;

        loop {
            let offset = self.tokenizer.offset();
            let token = self.tokenizer.next_or_eof().map_err(Error::into_fallable)?;

            let key = match token {
                Token::ObjectEnd => {
                    if state.config.allow_trailing_commas || state.delegate.object_is_empty(&object)
                    {
                        break;
                    }

                    return Err(Error {
                        offset,
                        kind: ErrorKind::IllegalTrailingComma,
                    });
                }
                Token::String(value) => {
                    state
                        .delegate
                        .object_key(&mut object, value)
                        .map_err(|kind| Error {
                            offset,
                            kind: ErrorKind::ErrorFromDelegate(kind),
                        })?
                }
                Token::Array | Token::Bool(_) | Token::Null | Token::Number(_) | Token::Object => {
                    return Err(Error {
                        offset,
                        kind: ErrorKind::ObjectKeysMustBeStrings,
                    });
                }
                _ => {
                    return Err(Error {
                        offset,
                        kind: ErrorKind::ExpectedObjectKey,
                    })
                }
            };

            let offset = self.tokenizer.offset();
            let token = self.tokenizer.next_or_eof().map_err(|mut error| {
                error.kind = ErrorKind::ExpectedColon;

                error.into_fallable()
            })?;

            if token != Token::Colon {
                return Err(Error {
                    offset,
                    kind: ErrorKind::ExpectedColon,
                });
            }

            let offset = self.tokenizer.offset();
            let value = self.read_tokens(state)?;
            state
                .delegate
                .object_value(&mut object, key, value)
                .map_err(|kind| Error {
                    offset,
                    kind: ErrorKind::ErrorFromDelegate(kind),
                })?;

            let offset = self.tokenizer.offset();
            let token = self.tokenizer.next_or_eof().map_err(Error::into_fallable)?;

            if token == Token::ObjectEnd {
                break;
            } else if token != Token::Comma {
                return Err(Error {
                    offset,
                    kind: ErrorKind::ExpectedCommaOrEndOfObject,
                });
            }
        }

        let object = state.delegate.end_object(object).map_err(|kind| Error {
            offset: self.tokenizer.offset(),
            kind: ErrorKind::ErrorFromDelegate(kind),
        })?;

        state.end_nest();

        Ok(object)
    }

    #[inline]
    fn read_array<D>(&mut self, state: &mut ParseState<'a, D>) -> Result<D::Value, Error<D::Error>>
    where
        D: ParseDelegate<'a>,
    {
        let offset = self.tokenizer.offset();
        let mut array = state.delegate.begin_array().map_err(|kind| Error {
            offset: self.tokenizer.offset(),
            kind: ErrorKind::ErrorFromDelegate(kind),
        })?;

        loop {
            let offset = self.tokenizer.offset();

            // a bit of a cheat for the sake of ultimate performance, instead of using an Option<Token>
            // we look at the char itself to determine the of a token
            if self.tokenizer.peek() == Some(PeekableTokenKind::ArrayEnd) {
                // commit the token
                self.tokenizer.source.offset += 1;

                if state.config.allow_trailing_commas || state.delegate.array_is_empty(&array) {
                    break;
                }

                return Err(Error {
                    offset,
                    kind: ErrorKind::IllegalTrailingComma,
                });
            }

            let value = self.read_tokens(state)?;

            state
                .delegate
                .array_value(&mut array, value)
                .map_err(|kind| Error {
                    offset: self.tokenizer.offset(),
                    kind: ErrorKind::ErrorFromDelegate(kind),
                })?;

            let offset = self.tokenizer.offset();
            let token = self.tokenizer.next_or_eof().map_err(Error::into_fallable)?;

            if token == Token::ArrayEnd {
                break;
            } else if token != Token::Comma {
                return Err(Error {
                    offset,
                    kind: ErrorKind::ExpectedCommaOrEndOfArray,
                });
            }
        }

        let array = state.delegate.end_array(array).map_err(|kind| Error {
            offset,
            kind: ErrorKind::ErrorFromDelegate(kind),
        })?;

        state.end_nest();

        Ok(array)
    }
}

/// A delegate for a [`Parser`].
///
/// This type has its functions invoked while a parser is parsing a JSON
/// payload.
pub trait ParseDelegate<'a> {
    /// The type that can represent all parsed JSON value types.
    type Value;
    /// The type that is used to represent a JSON object.
    type Object;
    /// The type that is used to represent a JSON array.
    type Array;
    /// The type that is used to represent the key of a field in a JSON object.
    type Key;
    /// The error type for this delegate.
    type Error;

    /// Returns the value representation of `null`.
    fn null(&mut self) -> Result<Self::Value, Self::Error>;
    /// Returns the value representation of a boolean.
    fn boolean(&mut self, value: bool) -> Result<Self::Value, Self::Error>;
    /// Returns the value representation of a [`JsonNumber`].
    fn number(&mut self, value: JsonNumber<'a>) -> Result<Self::Value, Self::Error>;
    /// Returns the value representation of a [`JsonString`].
    fn string(&mut self, value: JsonString<'a>) -> Result<Self::Value, Self::Error>;

    /// Returns an empty object.
    fn begin_object(&mut self) -> Result<Self::Object, Self::Error>;
    /// Processes the key for a new value in an object. Returns the key
    /// representation of the [`JsonString`].
    fn object_key(
        &mut self,
        object: &mut Self::Object,
        key: JsonString<'a>,
    ) -> Result<Self::Key, Self::Error>;
    /// Adds a new key-value pair to an object.
    fn object_value(
        &mut self,
        object: &mut Self::Object,
        key: Self::Key,
        value: Self::Value,
    ) -> Result<(), Self::Error>;
    /// Returns true if the object passed is empty.
    fn object_is_empty(&self, object: &Self::Object) -> bool;
    /// Returns the value representation of the object passed.
    fn end_object(&mut self, object: Self::Object) -> Result<Self::Value, Self::Error>;

    /// Returns an empty array.
    fn begin_array(&mut self) -> Result<Self::Array, Self::Error>;
    /// Adds a new value to an array.
    fn array_value(
        &mut self,
        array: &mut Self::Array,
        value: Self::Value,
    ) -> Result<(), Self::Error>;
    /// Returns true if the array passed is empty.
    fn array_is_empty(&self, array: &Self::Array) -> bool;
    /// Returns the value representation of the array passed.
    fn end_array(&mut self, array: Self::Array) -> Result<Self::Value, Self::Error>;

    /// Returns the [`JsonKind`] of `value`.
    fn kind_of(&self, value: &Self::Value) -> JsonKind;
}

struct ByteIterator<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Iterator for ByteIterator<'a> {
    type Item = (usize, &'a u8);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.bytes.get(self.offset);

        match token {
            None => None,
            Some(token) => {
                let offset = self.offset;
                self.offset += 1;

                Some((offset, token))
            }
        }
    }
}

impl<'a> ByteIterator<'a> {
    #[inline]
    pub fn new(slice: &'a [u8]) -> Self {
        Self {
            bytes: slice,
            offset: 0,
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&'a u8> {
        self.bytes.get(self.offset)
    }

    #[inline]
    fn next_non_ws(&mut self) -> Option<(usize, &'a u8)> {
        self.skip_ws();

        self.next()
    }

    #[inline]
    fn read_non_ws(&mut self) -> Result<(usize, &'a u8), Error> {
        self.skip_ws();

        self.read()
    }

    #[inline]
    fn skip_ws(&mut self) {
        loop {
            match self.bytes.get(self.offset) {
                Some(b' ' | b'\n' | b'\t' | b'\r') => {
                    self.offset += 1;
                }
                _ => {
                    return;
                }
            }
        }
    }

    #[inline]
    fn read(&mut self) -> Result<(usize, &'a u8), Error> {
        self.next().ok_or(Error {
            offset: self.offset,
            kind: ErrorKind::UnexpectedEof,
        })
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum InitialNumberState {
    Zero,
    Digit,
    Sign,
}

/// A JSON Value parsing configuration.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[non_exhaustive]
#[must_use]
pub struct ParseConfig {
    /// If true, allows trailing commas when parsing arrays and objects. If
    /// false, trailing commas will cause an [`ErrorKind::IllegalTrailingComma`]
    /// to be returned.
    pub allow_trailing_commas: bool,
    /// If present, nested arrays and objects will be limited to
    /// `recursion_limit` levels of nesting. If not present, no checks will be
    /// performed which can cause a stack overflow with very deeply nested
    /// payloads.
    pub recursion_limit: Option<usize>,
    /// If true, only arrays or objects will be allowed to parse at the root of
    /// the JSON payload.
    pub allow_all_types_at_root: bool,
}

impl Default for ParseConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseConfig {
    /// Returns the default configuration:
    ///
    /// ```rust
    /// let config = justjson::parser::ParseConfig::new();
    /// assert_eq!(config.allow_trailing_commas, false);
    /// assert_eq!(config.recursion_limit, Some(128));
    /// assert_eq!(config.allow_all_types_at_root, true);
    /// ```
    pub const fn new() -> Self {
        Self {
            allow_trailing_commas: false,
            recursion_limit: Some(128),
            allow_all_types_at_root: true,
        }
    }

    /// Returns a strict configuration, which differs from the default
    /// configuration by only allowing objects and arrays at the root:
    ///
    /// ```rust
    /// let config = justjson::parser::ParseConfig::strict();
    /// assert_eq!(config.allow_trailing_commas, false);
    /// assert_eq!(config.recursion_limit, Some(128));
    /// assert_eq!(config.allow_all_types_at_root, false);
    /// ```
    pub const fn strict() -> Self {
        Self {
            allow_trailing_commas: false,
            recursion_limit: Some(128),
            allow_all_types_at_root: false,
        }
    }

    /// Disables recursuion limit testing.
    ///
    /// Note: Malicious payloads may be able to cause stack overflows to occur
    /// if this is disabled.
    pub const fn without_recursion_limit(mut self) -> Self {
        self.recursion_limit = None;
        self
    }

    /// Sets the maximum recursion limit to `limit`.
    pub const fn with_recursion_limit(mut self, limit: usize) -> Self {
        self.recursion_limit = Some(limit);
        self
    }

    /// Sets whether to allow all types at the root of the JSON payload. If
    /// false, only arrays and objects will be allowed at the root of the JSON
    /// payload.
    pub const fn allowing_all_types_at_root(mut self, allow_all: bool) -> Self {
        self.allow_all_types_at_root = allow_all;
        self
    }

    /// Allows trailing commas when parsing objects and arrays.
    ///
    /// ```rust
    /// # #[cfg(feature = "alloc")]
    /// # fn wrapper() {
    /// use justjson::parser::ParseConfig;
    /// use justjson::Value;
    ///
    /// let source = r#"{"a":[true,],}"#;
    /// Value::from_json(source).expect_err("not enabled by default");
    /// let config = ParseConfig::new().allowing_trailing_commas();
    /// Value::from_json_with_config(source, config).expect("now parses");
    /// # }
    /// ```
    pub const fn allowing_trailing_commas(mut self) -> Self {
        self.allow_trailing_commas = true;
        self
    }
}

#[test]
fn config_test() {
    // Flip all the values of the strict config, and verify it matches.
    assert_eq!(
        ParseConfig::strict()
            .allowing_trailing_commas()
            .without_recursion_limit()
            .allowing_all_types_at_root(true),
        ParseConfig {
            allow_trailing_commas: true,
            recursion_limit: None,
            allow_all_types_at_root: true
        }
    );
}

#[derive(Debug)]
struct ParseState<'a, D>
where
    D: ParseDelegate<'a>,
{
    delegate: D,
    config: ParseConfig,
    remaining_depth: usize,
    _phantom: PhantomData<&'a ()>,
}

impl<'a, D> ParseState<'a, D>
where
    D: ParseDelegate<'a>,
{
    #[inline]
    pub fn begin_nest(&mut self) -> Result<(), ErrorKind<D::Error>> {
        if self.config.recursion_limit.is_some() {
            if self.remaining_depth > 0 {
                self.remaining_depth -= 1;
            } else {
                return Err(ErrorKind::RecursionLimitReached);
            }
        }

        Ok(())
    }

    #[inline]
    pub fn end_nest(&mut self) {
        if self.config.recursion_limit.is_some() {
            self.remaining_depth += 1;
        }
    }
}

impl<'a, D> ParseState<'a, D>
where
    D: ParseDelegate<'a>,
{
    fn new(config: ParseConfig, delegate: D) -> Self {
        Self {
            delegate,
            remaining_depth: config.recursion_limit.unwrap_or(usize::MAX),
            config,
            _phantom: PhantomData,
        }
    }
}

/// Every type supported by JSON.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum JsonKind {
    /// A null value.
    Null,
    /// A boolean value.
    Boolean,
    /// A numerical value.
    Number,
    /// A string value.
    String,
    /// A list of key-value pairs.
    Object,
    /// A list of values.
    Array,
}

impl<'a> ParseDelegate<'a> for () {
    type Array = usize;
    type Error = Infallible;
    type Key = ();
    type Object = usize;
    type Value = JsonKind;

    fn null(&mut self) -> Result<Self::Value, Self::Error> {
        Ok(JsonKind::Null)
    }

    fn boolean(&mut self, _value: bool) -> Result<Self::Value, Self::Error> {
        Ok(JsonKind::Boolean)
    }

    fn number(&mut self, _value: JsonNumber<'a>) -> Result<Self::Value, Self::Error> {
        Ok(JsonKind::Number)
    }

    fn string(&mut self, _value: JsonString<'a>) -> Result<Self::Value, Self::Error> {
        Ok(JsonKind::String)
    }

    fn begin_object(&mut self) -> Result<Self::Object, Self::Error> {
        Ok(0)
    }

    fn object_key(
        &mut self,
        _object: &mut Self::Object,
        _key: JsonString<'a>,
    ) -> Result<Self::Key, Self::Error> {
        Ok(())
    }

    fn object_value(
        &mut self,
        object: &mut Self::Object,
        _key: Self::Key,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        *object += 1;
        Ok(())
    }

    fn object_is_empty(&self, object: &Self::Object) -> bool {
        *object == 0
    }

    fn end_object(&mut self, _object: Self::Object) -> Result<Self::Value, Self::Error> {
        Ok(JsonKind::Object)
    }

    fn begin_array(&mut self) -> Result<Self::Array, Self::Error> {
        Ok(0)
    }

    fn array_value(
        &mut self,
        array: &mut Self::Array,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        *array += 1;
        Ok(())
    }

    fn array_is_empty(&self, array: &Self::Array) -> bool {
        *array == 0
    }

    fn end_array(&mut self, _array: Self::Array) -> Result<Self::Value, Self::Error> {
        Ok(JsonKind::Array)
    }

    fn kind_of(&self, value: &Self::Value) -> JsonKind {
        *value
    }
}

#[test]
fn validates() {
    assert_eq!(
        Parser::validate_json(r#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#),
        Ok(JsonKind::Object)
    );
    assert_eq!(
        Parser::validate_json_bytes(br#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#),
        Ok(JsonKind::Object)
    );
}

#[test]
#[cfg(feature = "alloc")]
fn tokenizes() {
    assert_eq!(
        Tokenizer::for_json("true")
            .collect::<Result<alloc::vec::Vec<_>, _>>()
            .unwrap(),
        &[Token::Bool(true)]
    );
    assert_eq!(
        Tokenizer::for_json_bytes(b"false")
            .collect::<Result<alloc::vec::Vec<_>, _>>()
            .unwrap(),
        &[Token::Bool(false)]
    );
}

#[test]
fn tokenizer_peek_tests() {
    let mut tokenizer = Tokenizer::for_json(r#"{"a": [1, true, false, null]} "#);

    for expected in [
        PeekableTokenKind::Object,
        PeekableTokenKind::String,
        PeekableTokenKind::Colon,
        PeekableTokenKind::Array,
        PeekableTokenKind::Number,
        PeekableTokenKind::Comma,
        PeekableTokenKind::True,
        PeekableTokenKind::Comma,
        PeekableTokenKind::False,
        PeekableTokenKind::Comma,
        PeekableTokenKind::Null,
        PeekableTokenKind::ArrayEnd,
        PeekableTokenKind::ObjectEnd,
    ] {
        assert_eq!(tokenizer.peek(), Some(expected));
        tokenizer.next().expect("eof").expect("invalid tokens");
    }
    assert_eq!(tokenizer.peek(), None);
    assert_eq!(tokenizer.next(), None);

    assert_eq!(
        Tokenizer::for_json("<").peek(),
        Some(PeekableTokenKind::Unrecognized)
    );
}
