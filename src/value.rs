use std::{io::Read, iter::Peekable, slice};

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
        Self::from_source(JsonSlice::from(json))
    }

    /// Parses a JSON value from `json`, returning a `Value<&str>` that borrows
    /// data from `json`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn from_json_bytes(json: &'a [u8]) -> Result<Self, Error> {
        Self::from_source(JsonSlice::from(json))
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
impl Value<String> {
    /// Parses a JSON value from `reader`, returning a `Value<String>`. This
    /// function cannot borrow data from `reader`.
    ///
    /// This function does not perform any buffering and reads one byte at a
    /// time. A buffered reader such as [`std::io::BufReader`] should almost
    /// certainly be used when calling this function.
    ///
    /// This function verifies that all bytes read from `reader` are valid UTF-8
    /// while parsing the JSON.
    pub fn from_reader<R: Read>(reader: R) -> Result<Self, Error> {
        Self::from_source(JsonReader::new(reader))
    }
}

impl<Backing> Value<Backing> {
    fn read_from_source<Source>(
        source: &mut Source,
        safe_strings: &[bool; 256],
    ) -> Result<Self, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        let value = match source.read_next_non_ws()? {
            (_, b'"') => {
                source.buffer_byte(b'"');
                Value::String(Self::read_string_from_source(source, safe_strings)?)
            }
            (_, b'{') => Value::Object(Self::read_object_from_source(source, safe_strings)?),
            (_, b'[') => Value::Array(Self::read_array_from_source(source, safe_strings)?),
            (offset, ch) if ch == b'-' || ch == b'+' => {
                source.buffer_byte(ch);
                Value::Number(Self::read_number_from_source(source, true, offset)?)
            }
            (offset, ch) if (b'0'..=b'9').contains(&ch) => {
                source.buffer_byte(ch);
                Value::Number(Self::read_number_from_source(source, false, offset)?)
            }
            (_, b't') => Self::read_literal_from_source(source, b"rue", Value::Boolean(true))?,
            (_, b'f') => Self::read_literal_from_source(source, b"alse", Value::Boolean(false))?,
            (_, b'n') => Self::read_literal_from_source(source, b"ull", Value::Null)?,
            (offset, other) => {
                return Err(Error {
                    offset,
                    kind: ErrorKind::Unexpected(other),
                })
            }
        };

        Ok(value)
    }

    fn from_source<Source>(mut source: Source) -> Result<Self, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        let safe_string_bytes = if source.guaranteed_utf8() {
            SAFE_STRING_BYTES
        } else {
            SAFE_STRING_BYTES_VERIFY_UTF8
        };

        let value = Self::read_from_source(&mut source, safe_string_bytes)?;

        match source.read_next_non_ws() {
            Err(err) if matches!(err.kind, ErrorKind::UnexpectedEof) => Ok(value),
            Ok((offset, _)) => Err(Error {
                offset,
                kind: ErrorKind::TrailingNonWhitespace,
            }),
            Err(other) => Err(other),
        }
    }

    fn read_object_from_source<Source>(
        source: &mut Source,
        safe_strings: &[bool; 256],
    ) -> Result<Object<Backing>, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        let inner = |source: &mut Source| {
            let mut object = Object::default();
            loop {
                let (offset, byte) = source.read_next_non_ws()?;

                let key = if byte == b'"' {
                    source.buffer_byte(b'"');
                    Self::read_string_from_source(source, safe_strings)?
                } else if byte == b'}' {
                    if object.0.is_empty() {
                        break;
                    }

                    return Err(Error {
                        offset,
                        kind: ErrorKind::ExpectedObjectKey,
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

                match source.read_next_non_ws() {
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

                let value = Self::read_from_source(source, safe_strings)?;

                object.0.push((key, value));
                match source.read_next_non_ws()? {
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
        inner(source).map_err(|mut err| {
            if matches!(err.kind, ErrorKind::UnexpectedEof) {
                err.kind = ErrorKind::UnclosedObject;
            }
            err
        })
    }

    fn read_array_from_source<Source>(
        source: &mut Source,
        safe_strings: &[bool; 256],
    ) -> Result<Vec<Value<Backing>>, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        let inner = |source: &mut Source| {
            let mut values = Vec::new();
            loop {
                let (offset, byte) = source
                    .peek_next_non_ws()?
                    .ok_or_else(|| source.read_byte().expect_err("eof expected"))?;

                let value = if byte == b']' {
                    if !values.is_empty() {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::IllegalTrailingComma,
                        });
                    }

                    source.read_byte()?;
                    break;
                } else {
                    Self::read_from_source(source, safe_strings)?
                };

                values.push(value);

                match source.read_next_non_ws()? {
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

        inner(source).map_err(|mut err| {
            if matches!(err.kind, ErrorKind::UnexpectedEof) {
                err.kind = ErrorKind::UnclosedArray;
            }
            err
        })
    }

    fn read_string_from_source<Source>(
        source: &mut Source,
        safe_strings: &[bool; 256],
    ) -> Result<JsonString<Backing>, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        let mut string_info = JsonStringInfo::NONE;
        loop {
            let (offset, byte) = source.read_byte_buffered().map_err(|mut err| {
                if matches!(err.kind, ErrorKind::UnexpectedEof) {
                    // Give a better error message
                    err.kind = ErrorKind::UnclosedString;
                }
                err
            })?;
            if safe_strings[usize::from(byte)] {
                string_info.add_bytes(1);
            } else {
                match byte {
                    b'"' => break Ok(source.take_string(string_info)),
                    b'\\' => match source.read_byte_buffered()? {
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
                                let (offset, digit) = source.read_byte_buffered()?;
                                let nibble = HEX_OFFSET_TABLE[usize::from(digit)];
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
                        let expected_bytes = byte.leading_ones() as usize;
                        if expected_bytes > 4 {
                            return Err(Error {
                                offset,
                                kind: ErrorKind::Unexpected(byte),
                            });
                        }

                        let mut utf8_bytes = [byte; 4];
                        for byte in utf8_bytes.iter_mut().take(expected_bytes).skip(1) {
                            *byte = source.read_byte_buffered()?.1;
                        }
                        string_info.add_bytes(expected_bytes);
                        if let Err(err) = std::str::from_utf8(&utf8_bytes[..expected_bytes]) {
                            return Err(Error {
                                offset,
                                kind: ErrorKind::from(err),
                            });
                        }
                    }
                    0..=31 => {
                        return Err(Error {
                            offset,
                            kind: ErrorKind::Unexpected(byte),
                        })
                    }
                    b' '..=127 => {
                        unreachable!()
                    }
                }
            }
        }
    }

    fn read_number_from_source<Source>(
        source: &mut Source,
        has_explicit_sign: bool,
        offset: usize,
    ) -> Result<JsonNumber<Backing>, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        // Numbers are the "hardest" in that we have to peek the digits since
        // there is no terminal character. Every other type in JSON has a way to
        // know when the type ends.

        // First, if the number began with a sign, we must read an integer
        // digit.
        let mut read_integer_digit = !has_explicit_sign;
        let mut last_offset = offset;
        while let Some((offset, byte)) = source.peek_byte()? {
            if byte.is_ascii_digit() {
                source.read_byte_buffered()?;
                last_offset = offset;
                read_integer_digit = true;
            } else {
                break;
            }
        }

        if !read_integer_digit {
            return Err(Error {
                offset: last_offset + 1,
                kind: ErrorKind::ExpectedDigit,
            });
        }

        // If the next character is a period, this is a floating point literal.
        if let Some((offset, b'.')) = source.peek_byte()? {
            source.read_byte_buffered()?;
            last_offset = offset;

            // Read one or more decimal digits
            let mut read_decimal_digit = false;
            while let Some((offset, byte)) = source.peek_byte()? {
                if byte.is_ascii_digit() {
                    source.read_byte_buffered()?;
                    last_offset = offset;
                    read_decimal_digit = true;
                } else {
                    break;
                }
            }

            // Next, we might have an exponent
            if let Some((offset, b'e' | b'E')) = source.peek_byte()? {
                source.read_byte_buffered()?;
                last_offset = offset;

                // Next, we might have a sign
                if let Some((offset, b'-' | b'+')) = source.peek_byte()? {
                    source.read_byte_buffered()?;
                    last_offset = offset;
                }

                // Read one or more exponent digits
                let mut read_exponent_digit = false;
                while let Some((offset, byte)) = source.peek_byte()? {
                    if byte.is_ascii_digit() {
                        source.read_byte_buffered()?;
                        last_offset = offset;
                        read_exponent_digit = true;
                    } else {
                        break;
                    }
                }

                if !read_exponent_digit {
                    return Err(Error {
                        offset: last_offset + 1,
                        kind: ErrorKind::ExpectedExponent,
                    });
                }
            } else if !read_decimal_digit {
                return Err(Error {
                    offset: last_offset + 1,
                    kind: ErrorKind::ExpectedDecimalDigit,
                });
            }

            Ok(source.take_number())
        } else {
            Ok(source.take_number())
        }
    }

    fn read_literal_from_source<Source>(
        source: &mut Source,
        remaining_bytes: &[u8],
        value: Self,
    ) -> Result<Self, Error>
    where
        Source: JsonSource<Backing = Backing>,
    {
        for expected in remaining_bytes {
            let (offset, byte) = source.read_byte()?;

            if &byte != expected {
                return Err(Error {
                    offset,
                    kind: ErrorKind::Unexpected(byte),
                });
            }
        }

        Ok(value)
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

pub trait JsonSource {
    type Backing;

    fn guaranteed_utf8(&self) -> bool;
    fn read_byte(&mut self) -> Result<(usize, u8), Error>;

    #[inline]
    fn read_next_non_ws(&mut self) -> Result<(usize, u8), Error> {
        loop {
            let (offset, byte) = self.read_byte()?;
            if !byte.is_ascii_whitespace() {
                break Ok((offset, byte));
            }
        }
    }

    fn read_byte_buffered(&mut self) -> Result<(usize, u8), Error>;
    fn peek_byte(&mut self) -> Result<Option<(usize, u8)>, Error>;
    fn peek_next_non_ws(&mut self) -> Result<Option<(usize, u8)>, Error>;

    fn buffer_byte(&mut self, byte: u8);
    fn take_string(&mut self, escape_info: JsonStringInfo) -> JsonString<Self::Backing>;
    fn take_number(&mut self) -> JsonNumber<Self::Backing>;
}

pub struct JsonSlice<'a> {
    from_str: bool,
    bytes: &'a [u8],
    iterator: Peekable<slice::Iter<'a, u8>>,
    offset: usize,
    token_start: Option<usize>,
}

impl<'a> JsonSlice<'a> {
    fn new(bytes: &'a [u8], from_str: bool) -> Self {
        Self {
            from_str,
            bytes,
            iterator: bytes.iter().peekable(),
            offset: 0,
            token_start: None,
        }
    }

    #[cold]
    fn error(&self, kind: ErrorKind) -> Error {
        Error {
            offset: self.offset,
            kind,
        }
    }
}

impl<'a> From<&'a [u8]> for JsonSlice<'a> {
    fn from(bytes: &'a [u8]) -> Self {
        Self::new(bytes, false)
    }
}

impl<'a> From<&'a str> for JsonSlice<'a> {
    fn from(source: &'a str) -> Self {
        Self::new(source.as_bytes(), true)
    }
}

impl<'a> JsonSource for JsonSlice<'a> {
    type Backing = &'a str;

    #[inline]
    fn read_byte(&mut self) -> Result<(usize, u8), Error> {
        if let Some(byte) = self.iterator.next() {
            let offset = self.offset;
            self.offset += 1;
            Ok((offset, *byte))
        } else {
            Err(self.error(ErrorKind::UnexpectedEof))
        }
    }

    #[inline]
    fn read_byte_buffered(&mut self) -> Result<(usize, u8), Error> {
        let offset = self.offset;
        let result = self.read_byte();

        if result.is_ok() && self.token_start.is_none() {
            self.token_start = Some(offset);
        }

        result
    }

    #[inline]
    fn peek_byte(&mut self) -> Result<Option<(usize, u8)>, Error> {
        if let Some(byte) = self.iterator.peek() {
            Ok(Some((self.offset, **byte)))
        } else {
            Ok(None)
        }
    }

    #[allow(unsafe_code)]
    #[inline]
    fn take_string(&mut self, info: JsonStringInfo) -> JsonString<&'a str> {
        let token_start = self
            .token_start
            .take()
            .expect("take_string called without buffering");
        JsonString {
            // SAFETY: The parser ensures that either Self::guaranteed_utf8()
            // returns true or that any non-ASCII bytes are valid UTF-8
            // sequences.
            source: unsafe { std::str::from_utf8_unchecked(&self.bytes[token_start..self.offset]) },
            info,
        }
    }

    #[allow(unsafe_code)]
    #[inline]
    fn take_number(&mut self) -> JsonNumber<&'a str> {
        let token_start = self
            .token_start
            .take()
            .expect("take_string called without buffering");
        JsonNumber {
            // SAFETY: The parser only allows ASCII characters through for
            // numbers.
            source: unsafe { std::str::from_utf8_unchecked(&self.bytes[token_start..self.offset]) },
        }
    }

    #[inline]
    fn buffer_byte(&mut self, _byte: u8) {
        self.token_start = Some(self.offset - 1);
    }

    #[inline]
    fn guaranteed_utf8(&self) -> bool {
        self.from_str
    }

    #[inline]
    fn peek_next_non_ws(&mut self) -> Result<Option<(usize, u8)>, Error> {
        if let Some(peeked) = self.iterator.peek() {
            if !peeked.is_ascii_whitespace() {
                return Ok(Some((self.offset, **peeked)));
            }
        }

        loop {
            // Eat the peeked value
            self.read_byte()?;

            if let Some((offset, peeked)) = self.peek_byte()? {
                if !peeked.is_ascii_whitespace() {
                    return Ok(Some((offset, peeked)));
                }
            } else {
                break Ok(None);
            }
        }
    }
}

pub struct JsonReader<R> {
    reader: R,
    offset: usize,
    peeked_byte: Option<(usize, u8)>,
    buffer: Vec<u8>,
}

impl<R> JsonReader<R>
where
    R: Read,
{
    pub const fn new(reader: R) -> Self {
        Self {
            reader,
            offset: 0,
            peeked_byte: None,
            buffer: Vec::new(),
        }
    }

    #[cold]
    fn error(&self, kind: ErrorKind) -> Error {
        Error {
            offset: self.offset,
            kind,
        }
    }
}

impl<R> JsonSource for JsonReader<R>
where
    R: Read,
{
    type Backing = String;

    #[inline]
    fn read_byte(&mut self) -> Result<(usize, u8), Error> {
        if let Some((offset, byte)) = self.peeked_byte.take() {
            Ok((offset, byte))
        } else {
            let offset = self.offset;
            let mut byte = 0;
            match self.reader.read_exact(slice::from_mut(&mut byte)) {
                Ok(()) => {
                    self.offset += 1;
                    Ok((offset, byte))
                }
                Err(other) => Err(self.error(ErrorKind::from(other))),
            }
        }
    }

    #[inline]
    fn read_byte_buffered(&mut self) -> Result<(usize, u8), Error> {
        match self.read_byte() {
            Ok((offset, byte)) => {
                self.buffer.push(byte);
                Ok((offset, byte))
            }
            other => other,
        }
    }

    #[inline]
    fn peek_byte(&mut self) -> Result<Option<(usize, u8)>, Error> {
        if let Some(byte) = self.peeked_byte {
            Ok(Some(byte))
        } else {
            let peeked = match self.read_byte() {
                Ok(read) => Some(read),
                Err(err) if matches!(err.kind, ErrorKind::UnexpectedEof) => None,
                Err(other) => return Err(other),
            };
            self.peeked_byte = peeked;
            Ok(peeked)
        }
    }

    #[inline]
    #[allow(unsafe_code)]
    fn take_string(&mut self, info: JsonStringInfo) -> JsonString<String> {
        let buffer = self.buffer.clone();
        self.buffer.clear();
        JsonString {
            // SAFETY: The parser ensures that either Self::guaranteed_utf8()
            // returns true or that any non-ASCII bytes are valid UTF-8
            // sequences.
            source: unsafe { String::from_utf8_unchecked(buffer) },
            info,
        }
    }

    #[inline]
    #[allow(unsafe_code)]
    fn take_number(&mut self) -> JsonNumber<String> {
        let buffer = self.buffer.clone();
        self.buffer.clear();
        JsonNumber {
            // SAFETY: The parser only allows ASCII characters through for
            // numbers.
            source: unsafe { String::from_utf8_unchecked(buffer) },
        }
    }

    #[inline]
    fn buffer_byte(&mut self, byte: u8) {
        self.buffer.push(byte);
    }

    #[inline]
    fn guaranteed_utf8(&self) -> bool {
        false
    }

    #[inline]
    fn peek_next_non_ws(&mut self) -> Result<Option<(usize, u8)>, Error> {
        if let Some((offset, peeked)) = self.peeked_byte {
            if !peeked.is_ascii_whitespace() {
                return Ok(Some((offset, peeked)));
            }
        }

        loop {
            // Eat the peeked value
            if self.peeked_byte.is_some() {
                self.read_byte()?;
            }

            let read = match self.read_byte() {
                Ok(read) => Some(read),
                Err(err) if matches!(err.kind, ErrorKind::UnexpectedEof) => None,
                Err(other) => return Err(other),
            };
            self.peeked_byte = read;
            if let Some((offset, peeked)) = read {
                if !peeked.is_ascii_whitespace() {
                    return Ok(Some((offset, peeked)));
                }
            } else {
                break Ok(None);
            }
        }
    }
}

#[test]
fn numbers() {
    assert_eq!(
        Value::from_source(JsonSlice::from("1")).unwrap(),
        Value::Number(JsonNumber { source: "1" })
    );
    assert_eq!(
        Value::from_source(JsonSlice::from("-1")).unwrap(),
        Value::Number(JsonNumber { source: "-1" })
    );
    assert_eq!(
        Value::from_source(JsonSlice::from("+1.0")).unwrap(),
        Value::Number(JsonNumber { source: "+1.0" })
    );
    assert_eq!(
        Value::from_source(JsonSlice::from("1.0e1")).unwrap(),
        Value::Number(JsonNumber { source: "1.0e1" })
    );
    assert_eq!(
        Value::from_source(JsonSlice::from("1.0e-10")).unwrap(),
        Value::Number(JsonNumber { source: "1.0e-10" })
    );
}
