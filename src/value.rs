use std::{
    fmt::{self, Display},
    iter::Peekable,
    slice,
};

use crate::{Error, ErrorKind, JsonNumber, JsonString, JsonStringInfo};

/// A JSON value.
///
/// The `Backing` generic is the storage mechanism used by [`JsonNumber`] and
/// [`JsonString`]. It generally is `&str` or `String`, depending on how the
/// JSON is being parsed.
#[derive(Debug, Eq, PartialEq)]
pub enum Value<Backing> {
    /// A JSON number.
    Number(JsonNumber<Backing>),
    /// A JSON string.
    String(JsonString<Backing>),
    /// A boolean value.
    Boolean(bool),
    /// A JSON object (key/value pairs).
    Object(Object<Backing>),
    /// A JSON array (list of values).
    Array(Vec<Value<Backing>>),
    /// A null value.
    Null,
}

impl<'a> Value<&'a str> {
    /// Parses a JSON value from `json`, returning a `Value<&str>` that borrows
    /// data from `json`.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn from_json(json: &'a str) -> Result<Self, Error> {
        Self::from_json_with_config(json, ParseConfig::default())
    }

    /// Parses a JSON value from `json` using the settings from`config`,
    /// returning a `Value<&str>` that borrows data from `json`.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn from_json_with_config(json: &'a str, config: ParseConfig) -> Result<Self, Error> {
        Self::from_bytes::<true>(json.as_bytes(), config)
    }

    /// Parses a JSON value from `json`, returning a `Value<&str>` that borrows
    /// data from `json`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    ///
    /// This function is equivalent to calling
    pub fn from_json_bytes(json: &'a [u8]) -> Result<Self, Error> {
        Self::from_json_bytes_with_config(json, ParseConfig::default())
    }

    /// Parses a JSON value from `json` using the settings from`config`,
    /// returning a `Value<&str>` that borrows data from `json`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn from_json_bytes_with_config(json: &'a [u8], config: ParseConfig) -> Result<Self, Error> {
        Self::from_bytes::<false>(json, config)
    }

    // pub fn into_owned(self) -> Value<'static> {
    //     match self {
    //         Value::Number(value) => Value::Number(value.into_owned()),
    //         Value::String(value) => Value::String(value.into_owned()),
    //         Value::Boolean(value) => Value::Boolean(value),
    //         Value::Null => Value::Null,
    //         Value::Object(object) => Value::Object(object.into_owned()),
    //         Value::Array(values) => {
    //             Value::Array(values.into_iter().map(Self::into_owned).collect())
    //         }
    //     }
    // }

    // pub fn to_owned(&self) -> Value<'static> {
    //     match self {
    //         Value::Number(value) => Value::Number(value.to_owned()),
    //         Value::String(value) => Value::String(value.to_owned()),
    //         Value::Boolean(value) => Value::Boolean(*value),
    //         Value::Null => Value::Null,
    //         Value::Object(object) => Value::Object(object.to_owned()),
    //         Value::Array(values) => Value::Array(values.iter().map(Self::to_owned).collect()),
    //     }
    // }
}

impl<'a> Value<&'a str> {
    fn read_from_source<const GUARANTEED_UTF8: bool>(
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
        state: &mut ParseState,
    ) -> Result<Self, Error> {
        let (offset, byte) = source.next_non_ws()?;
        Self::read_from_first_byte(offset, byte, source, state)
    }

    fn read_from_first_byte<const GUARANTEED_UTF8: bool>(
        offset: usize,
        first: &'a u8,
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
        state: &mut ParseState,
    ) -> Result<Self, Error> {
        let value = match (offset, first) {
            (offset, b'"') => Value::String(Self::read_string_from_source(source, offset)?),
            (offset, b'{') => {
                state.begin_nest().map_err(|kind| Error { offset, kind })?;
                Value::Object(Self::read_object_from_source(source, state)?)
            }
            (offset, b'[') => {
                state.begin_nest().map_err(|kind| Error { offset, kind })?;
                Value::Array(Self::read_array_from_source(source, state)?)
            }
            (offset, ch) if ch == &b'-' || ch == &b'+' => Value::Number(
                Self::read_number_from_source(source, InitialNumberState::Sign, offset)?,
            ),
            (offset, b'0') => Value::Number(Self::read_number_from_source(
                source,
                InitialNumberState::Zero,
                offset,
            )?),
            (offset, ch) if (b'1'..=b'9').contains(ch) => Value::Number(
                Self::read_number_from_source(source, InitialNumberState::Digit, offset)?,
            ),
            (_, b't') => Self::read_literal_from_source(source, b"rue", Value::Boolean(true))?,
            (_, b'f') => Self::read_literal_from_source(source, b"alse", Value::Boolean(false))?,
            (_, b'n') => Self::read_literal_from_source(source, b"ull", Value::Null)?,
            (offset, other) => {
                return Err(Error {
                    offset,
                    kind: ErrorKind::Unexpected(*other),
                })
            }
        };

        Ok(value)
    }

    fn from_bytes<const GUARANTEED_UTF8: bool>(
        source: &'a [u8],
        config: ParseConfig,
    ) -> Result<Self, Error> {
        let mut state = ParseState::from(config);
        let mut source = ByteIterator::<GUARANTEED_UTF8>::new(source);
        let value = Self::read_from_source(&mut source, &mut state)?;

        if !state.config.allow_all_types_at_root
            && !matches!(value, Value::Object(_) | Value::Array(_))
        {
            return Err(Error {
                offset: 0,
                kind: ErrorKind::PayloadsShouldBeObjectOrArray,
            });
        }

        match source.next_non_ws() {
            Err(err) if matches!(err.kind, ErrorKind::UnexpectedEof) => Ok(value),
            Ok((offset, _)) => Err(Error {
                offset,
                kind: ErrorKind::TrailingNonWhitespace,
            }),
            Err(other) => Err(other),
        }
    }

    fn read_object_from_source<const GUARANTEED_UTF8: bool>(
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
        state: &mut ParseState,
    ) -> Result<Object<&'a str>, Error> {
        let inner = |source: &mut ByteIterator<'a, GUARANTEED_UTF8>, state: &mut ParseState| {
            let mut object = Object::default();
            loop {
                let (offset, byte) = source.next_non_ws()?;

                let key = if byte == &b'"' {
                    Self::read_string_from_source(source, offset)?
                } else if byte == &b'}' {
                    if object.0.is_empty() && !state.config.allow_trailing_commas {
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

                match source.next_non_ws() {
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

                let value = Self::read_from_source(source, state)?;

                object.0.push((key, value));
                match source.next_non_ws()? {
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

            Ok(object)
        };
        let result = inner(source, state).map_err(|mut err| {
            if matches!(err.kind, ErrorKind::UnexpectedEof) {
                err.kind = ErrorKind::UnclosedObject;
            }
            err
        });
        state.end_nest();
        result
    }

    fn read_array_from_source<const GUARANTEED_UTF8: bool>(
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
        state: &mut ParseState,
    ) -> Result<Vec<Value<&'a str>>, Error> {
        let inner = |source: &mut ByteIterator<'a, GUARANTEED_UTF8>, state: &mut ParseState| {
            let mut values = Vec::new();
            loop {
                let (offset, byte) = source.next_non_ws()?;

                let value = if byte == &b']' {
                    if !values.is_empty() && !state.config.allow_trailing_commas {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::IllegalTrailingComma,
                        });
                    }

                    break;
                } else {
                    Self::read_from_first_byte(offset, byte, source, state)?
                };

                values.push(value);

                match source.next_non_ws()? {
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

            Ok(values)
        };

        let result = inner(source, state).map_err(|mut err| {
            if matches!(err.kind, ErrorKind::UnexpectedEof) {
                err.kind = ErrorKind::UnclosedArray;
            }
            err
        });
        state.end_nest();
        result
    }

    #[allow(unsafe_code)]
    fn read_string_from_source<const GUARANTEED_UTF8: bool>(
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
        start: usize,
    ) -> Result<JsonString<&'a str>, Error> {
        let safe_strings = if GUARANTEED_UTF8 {
            SAFE_STRING_BYTES
        } else {
            SAFE_STRING_BYTES_VERIFY_UTF8
        };
        let mut string_info = JsonStringInfo::NONE;

        loop {
            let (offset, byte) = source.next().ok_or(Error {
                offset: source.offset,
                kind: ErrorKind::UnclosedString,
            })?;
            if safe_strings[usize::from(*byte)] {
                string_info.add_bytes(1);
            } else {
                match byte {
                    b'"' => {
                        break Ok(JsonString {
                            source: unsafe {
                                std::str::from_utf8_unchecked(&source.bytes[start..=offset])
                            },
                            info: string_info,
                        })
                    }
                    b'\\' => match source.read()? {
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
                                let (offset, digit) = source.read()?;
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
                                let bytes = decoded.to_be_bytes();
                                let err =
                                    std::str::from_utf8(&bytes).expect_err("invalid codepoint");
                                return Err(Error {
                                    offset,
                                    kind: ErrorKind::from(err),
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
                        while let Some(byte) = source.peek() {
                            if byte < &&128 {
                                break;
                            }

                            source.next();
                        }

                        let unicode_end = source.offset;
                        string_info.add_bytes(unicode_end - utf8_start);
                        if let Err(err) =
                            std::str::from_utf8(&source.bytes[utf8_start..unicode_end])
                        {
                            // The offset on this is incorrect.
                            return Err(Error {
                                offset,
                                kind: ErrorKind::from(err),
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
    fn read_number_from_source<const GUARANTEED_UTF8: bool>(
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
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
            while let Some(byte) = source.peek() {
                if byte.is_ascii_digit() {
                    source.read()?;
                    read_integer_digit = true;
                } else {
                    break;
                }
            }

            if !read_integer_digit {
                return Err(Error {
                    offset: source.offset,
                    kind: ErrorKind::ExpectedDigit,
                });
            }
        }

        // If the next character is a period, this is a floating point literal.
        if let Some(b'.') = source.peek() {
            source.next();

            // Read one or more decimal digits
            let mut read_decimal_digit = false;
            while let Some(byte) = source.peek() {
                if byte.is_ascii_digit() {
                    source.next();
                    read_decimal_digit = true;
                } else {
                    break;
                }
            }

            if !read_decimal_digit {
                return Err(Error {
                    offset: source.offset,
                    kind: ErrorKind::ExpectedDecimalDigit,
                });
            }
        }

        // Next, we might have an exponent
        if let Some(b'e' | b'E') = source.peek() {
            source.next();

            // Next, we might have a sign
            if let Some(b'-' | b'+') = source.peek() {
                source.next();
            }

            // Read one or more exponent digits
            let mut read_exponent_digit = false;
            while let Some(byte) = source.peek() {
                if byte.is_ascii_digit() {
                    source.next();
                    read_exponent_digit = true;
                } else {
                    break;
                }
            }

            if !read_exponent_digit {
                return Err(Error {
                    offset: source.offset,
                    kind: ErrorKind::ExpectedExponent,
                });
            }
        }

        Ok(JsonNumber {
            source: unsafe { std::str::from_utf8_unchecked(&source.bytes[start..source.offset]) },
        })
    }

    fn read_literal_from_source<const GUARANTEED_UTF8: bool>(
        source: &mut ByteIterator<'a, GUARANTEED_UTF8>,
        remaining_bytes: &[u8],
        value: Self,
    ) -> Result<Self, Error> {
        for expected in remaining_bytes {
            let (offset, byte) = source.read()?;

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

impl<Backing> Value<Backing>
where
    Backing: AsRef<str>,
{
    fn write_json<W: fmt::Write, const PRETTY: bool>(
        &self,
        indentation: &str,
        line_ending: &str,
        destination: W,
    ) -> fmt::Result {
        let mut state = WriteState::<W, PRETTY>::new(destination, indentation, line_ending);

        self.write_json_value(&mut state)
    }

    fn write_json_value<W: fmt::Write, const PRETTY: bool>(
        &self,
        state: &mut WriteState<'_, W, PRETTY>,
    ) -> fmt::Result {
        match self {
            Value::String(string) => state.write(string.source.as_ref()),
            Value::Number(number) => state.write(number.source.as_ref()),
            Value::Boolean(bool) => state.write(if *bool { "true" } else { "false" }),
            Value::Null => state.write("null"),
            Value::Object(obj) => Self::write_json_object(obj, state),
            Value::Array(array) => Self::write_json_array(array, state),
        }
    }

    fn write_json_object<W: fmt::Write, const PRETTY: bool>(
        obj: &Object<Backing>,
        state: &mut WriteState<'_, W, PRETTY>,
    ) -> fmt::Result {
        state.begin_object()?;

        if !obj.0.is_empty() {
            state.new_line()?;
            for (index, (key, value)) in obj.0.iter().enumerate() {
                state.write(key.source.as_ref())?;
                state.write_colon()?;
                value.write_json_value(state)?;
                if index != obj.0.len() - 1 {
                    state.write(",")?;
                }
                state.new_line()?;
            }
        }

        state.end_object()
    }

    fn write_json_array<W: fmt::Write, const PRETTY: bool>(
        array: &Vec<Self>,
        state: &mut WriteState<'_, W, PRETTY>,
    ) -> fmt::Result {
        state.begin_array()?;

        if !array.is_empty() {
            state.new_line()?;
            for (index, value) in array.iter().enumerate() {
                value.write_json_value(state)?;
                if index != array.len() - 1 {
                    state.write(",")?;
                }
                state.new_line()?;
            }
        }

        state.end_array()
    }

    /// Converts this value to its JSON representation, with extra whitespace to
    /// make it easier for a human to read.
    ///
    /// This uses two spaces for indentation, and `\n` for end of lines. Use
    /// [`to_json_pretty_custom()`](Self::to_json_pretty_custom) to customize
    /// the formatting behavior.
    pub fn to_json_pretty(&self) -> String {
        let mut out = String::new();
        self.pretty_write_json_to(&mut out).expect("out of memory");
        out
    }

    /// Converts this value to its JSON representation, with extra whitespace to
    /// make it easier for a human to read.
    pub fn to_json_pretty_custom(&self, indentation: &str, line_ending: &str) -> String {
        let mut out = String::new();
        self.pretty_write_json_to_custom(indentation, line_ending, &mut out)
            .expect("out of memory");
        out
    }

    /// Converts this value to its JSON representation, with no extraneous
    /// whitespace.
    pub fn to_json(&self) -> String {
        let mut out = String::new();
        self.write_json_to(&mut out).expect("out of memory");
        out
    }

    /// Writes this value's JSON representation to `destination`, with no extraneous
    /// whitespace.
    pub fn write_json_to<W: fmt::Write>(&self, destination: W) -> fmt::Result {
        self.write_json::<W, false>("", "", destination)
    }

    /// Writes this value's JSON representation to `destination`, with extra
    /// whitespace to make it easier for a human to read.
    ///
    /// This uses two spaces for indentation, and `\n` for end of lines. Use
    /// [`to_json_pretty_custom()`](Self::to_json_pretty_custom) to customize
    /// the formatting behavior.
    pub fn pretty_write_json_to<W: fmt::Write>(&self, destination: W) -> fmt::Result {
        self.pretty_write_json_to_custom("  ", "\n", destination)
    }

    /// Writes this value's JSON representation to `destination`, with extra
    /// whitespace to make it easier for a human to read.
    pub fn pretty_write_json_to_custom<W: fmt::Write>(
        &self,
        indentation: &str,
        line_ending: &str,
        destination: W,
    ) -> fmt::Result {
        self.write_json::<W, true>(indentation, line_ending, destination)
    }
}

impl<Backing> Display for Value<Backing>
where
    Backing: AsRef<str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            self.pretty_write_json_to(f)
        } else {
            self.write_json_to(f)
        }
    }
}

struct WriteState<'a, W, const PRETTY: bool> {
    writer: W,
    level: usize,
    indent_per_level: &'a str,
    line_ending: &'a str,
    is_at_line_start: bool,
}

impl<'a, W, const PRETTY: bool> WriteState<'a, W, PRETTY>
where
    W: fmt::Write,
{
    fn new(writer: W, indentation: &'a str, line_ending: &'a str) -> Self {
        Self {
            writer,
            level: 0,
            is_at_line_start: true,
            indent_per_level: indentation,
            line_ending,
        }
    }

    fn write(&mut self, str: &str) -> fmt::Result {
        if PRETTY && self.is_at_line_start {
            self.is_at_line_start = false;

            for _ in 0..self.level {
                self.writer.write_str(self.indent_per_level)?;
            }
        }

        self.writer.write_str(str)?;
        Ok(())
    }

    fn new_line(&mut self) -> fmt::Result {
        if PRETTY {
            self.write(self.line_ending)?;
            self.is_at_line_start = true;
        }
        Ok(())
    }

    fn begin_object(&mut self) -> fmt::Result {
        self.write("{")?;
        self.level += 1;
        Ok(())
    }

    fn write_colon(&mut self) -> fmt::Result {
        if PRETTY {
            self.write(": ")?;
        } else {
            self.write(":")?;
        }
        Ok(())
    }

    fn end_object(&mut self) -> fmt::Result {
        self.level -= 1;
        self.write("}")?;
        Ok(())
    }

    fn begin_array(&mut self) -> fmt::Result {
        self.write("[")?;
        self.level += 1;
        Ok(())
    }

    fn end_array(&mut self) -> fmt::Result {
        self.level -= 1;
        self.write("]")?;
        Ok(())
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum InitialNumberState {
    Zero,
    Digit,
    Sign,
}

struct ByteIterator<'a, const GUARANTEED_UTF8: bool> {
    bytes: &'a [u8],
    offset: usize,
    iterator: Peekable<slice::Iter<'a, u8>>,
}

impl<'a, const GUARANTEED_UTF8: bool> Iterator for ByteIterator<'a, GUARANTEED_UTF8> {
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

impl<'a, const GUARANTEED_UTF8: bool> ByteIterator<'a, GUARANTEED_UTF8> {
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

static SAFE_STRING_BYTES_VERIFY_UTF8: &[bool; 256] = {
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

impl<'a> PartialEq<Value<&'a str>> for Value<String> {
    fn eq(&self, other: &Value<&'a str>) -> bool {
        match (self, other) {
            (Self::Number(l0), Value::Number(r0)) => l0 == r0,
            (Self::String(l0), Value::String(r0)) => l0 == r0,
            (Self::Boolean(l0), Value::Boolean(r0)) => l0 == r0,
            (Self::Object(l0), Value::Object(r0)) => l0 == r0,
            (Self::Array(l0), Value::Array(r0)) => l0 == r0,
            (Self::Null, Value::Null) => true,
            _ => false,
        }
    }
}

/// A JSON Object (list of key-value pairs).
#[derive(Debug, Eq, PartialEq)]
pub struct Object<Backing>(Vec<(JsonString<Backing>, Value<Backing>)>);

impl<Backing> Default for Object<Backing> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Backing> Object<Backing> {
    /// Returns an empty object.
    #[must_use]
    pub const fn new() -> Self {
        Self(Vec::new())
    }
}

// impl<'a> Object<'a> {
//     pub fn into_owned(self) -> Object<'static> {
//         Object(
//             self.0
//                 .into_iter()
//                 .map(|(key, value)| (key.into_owned(), value.into_owned()))
//                 .collect(),
//         )
//     }

//     pub fn to_owned(&self) -> Object<'static> {
//         Object(
//             self.0
//                 .iter()
//                 .map(|(key, value)| (key.to_owned(), value.to_owned()))
//                 .collect(),
//         )
//     }
// }

impl<Backing> FromIterator<(JsonString<Backing>, Value<Backing>)> for Object<Backing> {
    fn from_iter<T: IntoIterator<Item = (JsonString<Backing>, Value<Backing>)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a> PartialEq<Object<&'a str>> for Object<String> {
    fn eq(&self, other: &Object<&'a str>) -> bool {
        self.0.len() == other.0.len()
            && self
                .0
                .iter()
                .zip(other.0.iter())
                .all(|(a, b)| a.0 == b.0 && a.1 == b.1)
    }
}

#[test]
fn primitive_values() {
    assert_eq!(Value::from_json("true").unwrap(), Value::Boolean(true));
    assert_eq!(Value::from_json("false").unwrap(), Value::Boolean(false));
    assert_eq!(Value::from_json("null").unwrap(), Value::Null);
}

#[test]
fn objects() {
    assert_eq!(
        Value::from_json("{}").unwrap(),
        Value::Object(Object::default())
    );
    assert_eq!(
        Value::from_json(r#"{"hello":"world"}"#).unwrap(),
        Value::Object(Object::from_iter([(
            JsonString::from_json(r#""hello""#).unwrap(),
            Value::String(JsonString::from_json(r#""world""#).unwrap())
        )]))
    );
    assert_eq!(
        Value::from_json(r#" { "hello" : "world" , "another" : "value" }"#).unwrap(),
        Value::Object(Object::from_iter([
            (
                JsonString::from_json(r#""hello""#).unwrap(),
                Value::String(JsonString::from_json(r#""world""#).unwrap())
            ),
            (
                JsonString::from_json(r#""another""#).unwrap(),
                Value::String(JsonString::from_json(r#""value""#).unwrap())
            )
        ]))
    );
}

#[test]
fn numbers() {
    assert_eq!(
        Value::from_json("1").unwrap(),
        Value::Number(JsonNumber { source: "1" })
    );
    assert_eq!(
        Value::from_json("-1").unwrap(),
        Value::Number(JsonNumber { source: "-1" })
    );
    assert_eq!(
        Value::from_json("+1.0").unwrap(),
        Value::Number(JsonNumber { source: "+1.0" })
    );
    assert_eq!(
        Value::from_json("1.0e1").unwrap(),
        Value::Number(JsonNumber { source: "1.0e1" })
    );
    assert_eq!(
        Value::from_json("1.0e-10").unwrap(),
        Value::Number(JsonNumber { source: "1.0e-10" })
    );
}

/// A JSON Value parsing configuration.
#[derive(Debug, Clone, Copy)]
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
    /// let config = justjson::ParseConfig::new();
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
    /// let config = justjson::ParseConfig::strict();
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
}

#[derive(Debug)]
struct ParseState {
    config: ParseConfig,
    remaining_depth: usize,
}

impl ParseState {
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

impl From<ParseConfig> for ParseState {
    fn from(config: ParseConfig) -> Self {
        Self {
            remaining_depth: config.recursion_limit.unwrap_or(usize::MAX),
            config,
        }
    }
}
