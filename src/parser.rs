use core::slice;
use std::iter::Peekable;
use std::marker::PhantomData;

use crate::{Error, ErrorKind, JsonNumber, JsonString, JsonStringInfo};

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
    source: ByteIterator<'a>,
}

impl<'a> Parser<'a, false> {
    /// Parses a JSON payload, invoking functions on `delegate` as the payload
    /// is parsed.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn parse_json_bytes<D>(value: &'a [u8], delegate: D) -> Result<D::Value, Error>
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
    ) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        Self::parse_bytes(value, config, delegate)
    }
}

impl<'a> Parser<'a, true> {
    /// Parses a JSON payload, invoking functions on `delegate` as the payload
    /// is parsed.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn parse_json<D>(value: &'a str, delegate: D) -> Result<D::Value, Error>
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
    ) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        Self::parse_bytes(value.as_bytes(), config, delegate)
    }
}

impl<'a, const GUARANTEED_UTF8: bool> Parser<'a, GUARANTEED_UTF8> {
    fn parse_bytes<D>(source: &'a [u8], config: ParseConfig, delegate: D) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        let mut state = ParseState::new(config, delegate);
        let mut parser = Self {
            source: ByteIterator::new(source),
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

        match parser.source.next_non_ws() {
            Err(err) => {
                // The only error that next_non_ws can return is an unexpected
                // eof.
                debug_assert!(matches!(err.kind, ErrorKind::UnexpectedEof));
                Ok(value)
            }
            Ok((offset, _)) => Err(Error {
                offset,
                kind: ErrorKind::TrailingNonWhitespace,
            }),
        }
    }

    fn read_from_source<D>(&mut self, state: &mut ParseState<'a, D>) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        let (offset, byte) = self.source.next_non_ws()?;
        self.read_from_first_byte(offset, byte, state)
    }

    fn read_from_first_byte<D>(
        &mut self,
        offset: usize,
        first: &'a u8,
        state: &mut ParseState<'a, D>,
    ) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        let value = match (offset, first) {
            (offset, b'"') => state.delegate.string(self.read_string_from_source(offset)?),
            (offset, b'{') => {
                state.begin_nest().map_err(|kind| Error { offset, kind })?;
                self.read_object_from_source(state)?
            }
            (offset, b'[') => {
                state.begin_nest().map_err(|kind| Error { offset, kind })?;
                self.read_array_from_source(state)?
            }
            (offset, ch) if ch == &b'-' || ch == &b'+' => state
                .delegate
                .number(self.read_number_from_source(InitialNumberState::Sign, offset)?),
            (offset, b'0') => state
                .delegate
                .number(self.read_number_from_source(InitialNumberState::Zero, offset)?),
            (offset, ch) if (b'1'..=b'9').contains(ch) => state
                .delegate
                .number(self.read_number_from_source(InitialNumberState::Digit, offset)?),
            (_, b't') => {
                self.read_literal_from_source::<D>(b"rue", state.delegate.boolean(true))?
            }
            (_, b'f') => {
                self.read_literal_from_source::<D>(b"alse", state.delegate.boolean(false))?
            }
            (_, b'n') => self.read_literal_from_source::<D>(b"ull", state.delegate.null())?,
            (offset, other) => {
                return Err(Error {
                    offset,
                    kind: ErrorKind::Unexpected(*other),
                })
            }
        };

        Ok(value)
    }

    fn read_object_from_source<D>(
        &mut self,
        state: &mut ParseState<'a, D>,
    ) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        let mut inner = || {
            let mut obj = state.delegate.begin_object();
            loop {
                let (offset, byte) = self.source.next_non_ws()?;

                let key = if byte == &b'"' {
                    self.read_string_from_source(offset)?
                } else if byte == &b'}' {
                    if state.delegate.object_is_empty(&obj) || state.config.allow_trailing_commas {
                        break;
                    }

                    return Err(Error {
                        offset,
                        kind: ErrorKind::IllegalTrailingComma,
                    });
                } else if matches!(byte, b'+' | b'-' | b'{' | b'[' | b't' | b'f' | b'n')
                    || byte.is_ascii_digit()
                {
                    return Err(Error {
                        offset,
                        kind: ErrorKind::ObjectKeysMustBeStrings,
                    });
                } else {
                    return Err(Error {
                        offset,
                        kind: ErrorKind::ExpectedObjectKey,
                    });
                };

                let key = state.delegate.object_key(&mut obj, key);

                match self.source.next_non_ws() {
                    Ok((_, b':')) => {}
                    Ok((offset, _)) => {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::ExpectedColon,
                        })
                    }
                    Err(mut other) => {
                        other.kind = ErrorKind::ExpectedColon;
                        return Err(other);
                    }
                }

                let value = self.read_from_source(state)?;
                state.delegate.object_value(&mut obj, key, value);

                match self.source.next_non_ws()? {
                    (_, b',') => {}
                    (_, b'}') => break,
                    (offset, _) => {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::ExpectedCommaOrEndOfObject,
                        })
                    }
                }
            }

            Ok(state.delegate.end_object(obj))
        };
        let result = inner().map_err(|mut err| {
            if matches!(err.kind, ErrorKind::UnexpectedEof) {
                err.kind = ErrorKind::UnclosedObject;
            }
            err
        });
        state.end_nest();
        result
    }

    fn read_array_from_source<D>(
        &mut self,
        state: &mut ParseState<'a, D>,
    ) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        let mut inner = || {
            let mut array = state.delegate.begin_array();
            loop {
                let (offset, byte) = self.source.next_non_ws()?;

                let value = if byte == &b']' {
                    if state.delegate.array_is_empty(&array) || state.config.allow_trailing_commas {
                        break;
                    }

                    return Err(Error {
                        offset,
                        kind: ErrorKind::IllegalTrailingComma,
                    });
                } else {
                    self.read_from_first_byte(offset, byte, state)?
                };

                state.delegate.array_value(&mut array, value);

                match self.source.next_non_ws()? {
                    (_, b',') => {}
                    (_, b']') => break,
                    (offset, _) => {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::ExpectedCommaOrEndOfArray,
                        })
                    }
                }
            }

            Ok(state.delegate.end_array(array))
        };

        let result = inner().map_err(|mut err| {
            if matches!(err.kind, ErrorKind::UnexpectedEof) {
                err.kind = ErrorKind::UnclosedArray;
            }
            err
        });
        state.end_nest();
        result
    }

    #[allow(unsafe_code)]
    fn read_string_from_source(&mut self, start: usize) -> Result<JsonString<&'a str>, Error> {
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
                            source: unsafe {
                                std::str::from_utf8_unchecked(&self.source.bytes[start..=offset])
                            },
                            info: string_info,
                        })
                    }
                    b'\\' => match self.source.read()? {
                        (_, ch)
                            if matches!(
                                ch,
                                b'"' | b'\\' | b'/' | b'b' | b'f' | b'r' | b'n' | b't'
                            ) =>
                        {
                            string_info.add_bytes_from_escape(1);
                        }
                        (offset, b'u') => {
                            // 4 hexadecimal digits.
                            let mut decoded = 0_u16;
                            for _ in 0..4 {
                                let (offset, digit) = self.source.read()?;
                                let nibble = HEX_OFFSET_TABLE[usize::from(*digit)];
                                if nibble == u8::MAX {
                                    return Err(Error {
                                        offset,
                                        kind: ErrorKind::InvalidHexadecimal,
                                    });
                                }
                                decoded = decoded << 4 | u16::from(nibble);
                            }
                            if let Some(ch) = char::from_u32(u32::from(decoded)) {
                                string_info.add_bytes_from_escape(ch.len_utf8());
                            } else {
                                // Produce a Utf8 error.
                                return Err(Error {
                                    offset,
                                    kind: ErrorKind::Utf8,
                                });
                            }
                        }
                        (offset, _) => {
                            return Err(Error {
                                offset,
                                kind: ErrorKind::InvalidEscape,
                            })
                        }
                    },
                    128..=255 => {
                        // Manual UTF-8 validation
                        let utf8_start = offset;
                        while let Some(byte) = self.source.peek() {
                            if byte < &&128 {
                                break;
                            }

                            self.source.next();
                        }

                        let unicode_end = self.source.offset;
                        string_info.add_bytes(unicode_end - utf8_start);
                        if std::str::from_utf8(&self.source.bytes[utf8_start..unicode_end]).is_err()
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
                        unreachable!()
                    }
                }
            }
        }
    }

    #[allow(unsafe_code)]
    fn read_number_from_source(
        &mut self,
        initial_state: InitialNumberState,
        start: usize,
    ) -> Result<JsonNumber<&'a str>, Error> {
        // Numbers are the "hardest" in that we have to peek the digits since
        // there is no terminal character. Every other type in JSON has a way to
        // know when the type ends.

        // First, if the number began with a sign, we must read an integer
        // digit. The JSON spec disallows numbers with leading 0s. If the first
        // digit is a 0, it must be a decimal with a 0 integer value.
        if initial_state != InitialNumberState::Zero {
            let mut read_integer_digit = initial_state == InitialNumberState::Digit;
            while let Some(byte) = self.source.peek() {
                if byte.is_ascii_digit() {
                    self.source.read()?;
                    read_integer_digit = true;
                } else {
                    break;
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
            source: unsafe {
                std::str::from_utf8_unchecked(&self.source.bytes[start..self.source.offset])
            },
        })
    }

    fn read_literal_from_source<D>(
        &mut self,
        remaining_bytes: &[u8],
        value: D::Value,
    ) -> Result<D::Value, Error>
    where
        D: ParseDelegate<'a>,
    {
        for expected in remaining_bytes {
            let (offset, byte) = self.source.read()?;

            if byte != expected {
                return Err(Error {
                    offset,
                    kind: ErrorKind::Unexpected(*byte),
                });
            }
        }

        Ok(value)
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

    /// Returns the value representation of `null`.
    fn null(&mut self) -> Self::Value;
    /// Returns the value representation of a boolean.
    fn boolean(&mut self, value: bool) -> Self::Value;
    /// Returns the value representation of a [`JsonNumber`].
    fn number(&mut self, value: JsonNumber<&'a str>) -> Self::Value;
    /// Returns the value representation of a [`JsonString`].
    fn string(&mut self, value: JsonString<&'a str>) -> Self::Value;

    /// Returns an empty object.
    fn begin_object(&mut self) -> Self::Object;
    /// Processes the key for a new value in an object. Returns the key
    /// representation of the [`JsonString`].
    fn object_key(&mut self, object: &mut Self::Object, key: JsonString<&'a str>) -> Self::Key;
    /// Adds a new key-value pair to an object.
    fn object_value(&mut self, object: &mut Self::Object, key: Self::Key, value: Self::Value);
    /// Returns true if the object passed is empty.
    fn object_is_empty(&self, object: &Self::Object) -> bool;
    /// Returns the value representation of the object passed.
    fn end_object(&mut self, object: Self::Object) -> Self::Value;

    /// Returns an empty array.
    fn begin_array(&mut self) -> Self::Array;
    /// Adds a new value to an array.
    fn array_value(&mut self, array: &mut Self::Array, value: Self::Value);
    /// Returns true if the array passed is empty.
    fn array_is_empty(&self, array: &Self::Array) -> bool;
    /// Returns the value representation of the array passed.
    fn end_array(&mut self, array: Self::Array) -> Self::Value;

    /// Returns the [`JsonKind`] of `value`.
    fn kind_of(&self, value: &Self::Value) -> JsonKind;
}

struct ByteIterator<'a> {
    bytes: &'a [u8],
    offset: usize,
    iterator: Peekable<slice::Iter<'a, u8>>,
}

impl<'a> Iterator for ByteIterator<'a> {
    type Item = (usize, &'a u8);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next().map(|b| {
            let offset = self.offset;
            self.offset += 1;
            (offset, b)
        })
    }
}

impl<'a> ByteIterator<'a> {
    #[inline]
    pub fn new(slice: &'a [u8]) -> Self {
        Self {
            bytes: slice,
            offset: 0,
            iterator: slice.iter().peekable(),
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&&'a u8> {
        self.iterator.peek()
    }

    #[inline]
    fn next_non_ws(&mut self) -> Result<(usize, &'a u8), Error> {
        loop {
            match self.read()? {
                (_, b' ' | b'\n' | b'\t' | b'\r') => {}
                other => return Ok(other),
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
    /// use justjson::parser::ParseConfig;
    /// use justjson::Value;
    ///
    /// let source = r#"{"a":[true,],}"#;
    /// Value::from_json(source).expect_err("not enabled by default");
    /// let config = ParseConfig::new().allowing_trailing_commas();
    /// Value::from_json_with_config(source, config).expect("now parses");
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
    pub fn begin_nest(&mut self) -> Result<(), ErrorKind> {
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

#[allow(clippy::inconsistent_digit_grouping)]
static HEX_OFFSET_TABLE: [u8; 256] = {
    const ERR: u8 = u8::MAX;
    [
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 0
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 1
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 2
        0__, 1__, 2__, 3__, 4__, 5__, 6__, 7__, 8__, 9__, ERR, ERR, ERR, ERR, ERR, ERR, // 3
        ERR, 10_, 11_, 12_, 13_, 14_, 15_, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 4
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 5
        ERR, 10_, 11_, 12_, 13_, 14_, 15_, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 6
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 7
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 8
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 9
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // A
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // B
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // C
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // D
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // E
        ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // F
    ]
};

static SAFE_STRING_BYTES: &[bool; 256] = {
    const ER: bool = false;
    const UC: bool = true;
    const BS: bool = false;
    const QU: bool = false;
    const __: bool = true;
    //  0   1   2   3   4   5   6   7   8   9   A   B   C    D   E   F
    &[
        ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, // 0
        ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, // 1
        __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
        __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // 8
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // 9
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // A
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // B
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // C
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // D
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // E
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // F
    ]
};

static SAFE_STRING_BYTES_VERIFY_UTF8: &[bool; 256] = {
    const ER: bool = false;
    const UC: bool = false;
    const BS: bool = false;
    const QU: bool = false;
    const __: bool = true;
    //  0   1   2   3   4   5   6   7   8   9   A   B   C    D   E   F
    &[
        ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, // 0
        ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, ER, // 1
        __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
        __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // 8
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // 9
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // A
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // B
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // C
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // D
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // E
        UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, // F
    ]
};

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
