use crate::{
    error::{Error, ErrorKind},
    value::Value,
};

/// A JSON-encoded string.
// TODO document comparisons
#[derive(Debug, Eq, PartialEq, Clone)]

pub struct JsonString<Backing> {
    /// The JSON-source for the string.
    pub source: Backing,
    pub(crate) info: JsonStringInfo,
}

impl<'a> JsonString<&'a str> {
    // pub fn into_owned(self) -> JsonString<'static> {
    //     JsonString {
    //         source: Cow::Owned(match self.source {
    //             Cow::Borrowed(value) => value.to_string(),
    //             Cow::Owned(value) => value,
    //         }),
    //         has_escapes: self.has_escapes,
    //     }
    // }

    // pub fn to_owned(&self) -> JsonString<'static> {
    //     JsonString {
    //         source: Cow::Owned(match &self.source {
    //             Cow::Borrowed(value) => value.to_string(),
    //             Cow::Owned(value) => value.clone(),
    //         }),
    //         has_escapes: self.has_escapes,
    //     }
    // }

    /// Parses `json`, expecting a single string value.
    ///
    /// # Errors
    ///
    /// Returns [`ErrorKind::ExpectedString`] if a non-string value is
    /// encountered.
    pub fn from_json(json: &'a str) -> std::result::Result<Self, Error> {
        if let Value::String(str) = Value::from_json(json)? {
            Ok(str)
        } else {
            Err(Error {
                offset: 0,
                kind: ErrorKind::ExpectedString,
            })
        }
    }
}

impl<'a> PartialEq<JsonString<&'a str>> for JsonString<String> {
    fn eq(&self, other: &JsonString<&'a str>) -> bool {
        self.source == other.source
    }
}

impl<'a, 'b, T> PartialEq<&'a str> for &'b JsonString<T>
where
    T: AsRef<str>,
{
    fn eq(&self, other: &&'a str) -> bool {
        (*self) == other
    }
}

impl<'a, T> PartialEq<&'a str> for JsonString<T>
where
    T: AsRef<str>,
{
    #[allow(unsafe_code)]
    fn eq(&self, other: &&'a str) -> bool {
        let unescaped_length = self.info.unescaped_length();
        let source = self.source.as_ref();
        if self.info.has_escapes() {
            // Quick check, if the decoded length differs, the strings can't be equal
            if unescaped_length != other.len() {
                return false;
            }

            let mut other_chars = other.chars();
            let mut our_chars = source[1..source.len() - 1].char_indices();
            while let Some((_, ch)) = our_chars.next() {
                let other_ch = other_chars.next().expect("length matches");
                let matches = if ch == '\\' {
                    match our_chars.next().expect("already validated") {
                        (_, 'r') => '\r' == other_ch,
                        (_, 'n') => '\n' == other_ch,
                        (_, 't') => '\t' == other_ch,
                        (_, 'b') => '\x07' == other_ch,
                        (_, 'f') => '\x0c' == other_ch,
                        (offset, 'u') => {
                            // four hex digits
                            for _ in 0..4 {
                                our_chars.next();
                            }
                            // We access the string slice directly rather than
                            // trying to rebuild via char indicies for speed.
                            let hex = &source[offset + 2..offset + 6];
                            let value = u16::from_str_radix(hex, 16).expect("already validated");

                            // SAFETY: The JSON string has already had its UTF
                            // escapes validated.
                            let ch = unsafe { char::from_u32_unchecked(u32::from(value)) };
                            ch == other_ch
                        }
                        (_, other) => other == other_ch,
                    }
                } else {
                    ch == other_ch
                };
                if !matches {
                    return false;
                }
            }

            true
        } else {
            // Direct string comparison excluding the quotes.
            &source[1..=unescaped_length] == *other
        }
    }
}

#[test]
fn json_string_from_json() {
    assert_eq!(
        JsonString::from_json(r#""Hello, World!""#).unwrap(),
        JsonString {
            source: r#""Hello, World!""#,
            info: JsonStringInfo::new(false, 13),
        }
    );

    let expected_string = JsonString::from_json(r#"true"#)
        .expect_err("shouldn't allow non-strings")
        .kind;
    assert!(matches!(expected_string, ErrorKind::ExpectedString));
}

#[test]
fn json_string_cmp() {
    fn test_json(json: &str, expected: &str) {
        assert_eq!(JsonString::from_json(json).unwrap(), expected);
    }

    test_json(r#""Hello, World!""#, "Hello, World!");
    test_json(r#""\"\\\/\b\f\n\r\t\u25eF""#, "\"\\/\x07\x0c\n\r\t\u{25ef}");
    test_json("\"\u{25ef}\"", "\u{25ef}");
}

/// Information about a [`JsonString`]:
///
/// - Whether any escape sequences are in the source
/// - The length of the String if the escape sequences are decoded.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct JsonStringInfo(usize);

impl JsonStringInfo {
    #[cfg(target_pointer_width = "32")]
    const UNESCAPED_BYTES_MASK: usize = 0x7FFF_FFFF;
    #[cfg(target_pointer_width = "64")]
    const UNESCAPED_BYTES_MASK: usize = 0x7FFF_FFFF_FFFF_FFFF;

    const HAS_ESCAPES_MASK: usize = usize::MAX ^ Self::UNESCAPED_BYTES_MASK;

    /// Information with no escapes and 0 length.
    pub const NONE: Self = Self::new(false, 0);

    /// Returns a new instance with the initial values provided.
    #[must_use]
    pub const fn new(has_escapes: bool, unescaped_length: usize) -> Self {
        // assert_eq!(
        //     unescaped_length & Self::HAS_ESCAPES_MASK,
        //     0,
        //     "too many bytes needed"
        // );

        if has_escapes {
            Self(Self::HAS_ESCAPES_MASK | unescaped_length)
        } else {
            Self(unescaped_length)
        }
    }

    /// Returns whether the string has any escape sequences.
    #[must_use]
    pub const fn has_escapes(self) -> bool {
        self.0 & Self::HAS_ESCAPES_MASK != 0
    }

    /// Returns the length of the string after decoding any escape sequences.
    #[must_use]
    pub const fn unescaped_length(self) -> usize {
        self.0 & Self::UNESCAPED_BYTES_MASK
    }

    pub(crate) fn add_bytes(&mut self, bytes: usize) {
        self.0 = self.0.checked_add(bytes).expect("too many extra bytes");
    }

    pub(crate) fn add_bytes_from_escape(&mut self, bytes: usize) {
        self.0 = (Self::HAS_ESCAPES_MASK | self.0)
            .checked_add(bytes)
            .expect("too many extra bytes");
    }
}

impl std::fmt::Debug for JsonStringInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EscapeInfo")
            .field("has_escapes", &self.has_escapes())
            .field("unescaped_length", &self.unescaped_length())
            .finish()
    }
}
