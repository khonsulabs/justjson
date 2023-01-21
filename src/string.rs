use std::{
    borrow::Cow,
    str::{CharIndices, Chars},
};

use crate::{
    error::{Error, ErrorKind},
    value::Value,
};

/// A JSON-encoded string.
// TODO document comparisons
#[derive(Debug, Eq, PartialEq, Clone)]

pub struct JsonString<'a> {
    /// The JSON-source for the string.
    pub(crate) source: Cow<'a, str>,
    pub(crate) info: JsonStringInfo,
}

impl<'a> JsonString<'a> {
    /// Parses `json`, expecting a single string value.
    ///
    /// # Errors
    ///
    /// Returns [`ErrorKind::ExpectedString`] if a non-string value is
    /// encountered.
    pub fn from_json(json: &'a str) -> Result<Self, Error> {
        if let Value::String(str) = Value::from_json(json)? {
            Ok(str)
        } else {
            Err(Error {
                offset: 0,
                kind: ErrorKind::ExpectedString,
            })
        }
    }

    /// Returns the contained string after decoding any escape sequences, if
    /// needed. If there are no escape sequences, the contents of the string are
    /// borrowed.
    ///
    /// This type keeps track of whether the underlying string contains escape
    /// sequences, so this function is nearly instananeous when there are no
    /// escape sequences.
    #[must_use]
    pub fn decode_if_needed(&self) -> Cow<'_, str> {
        if self.info.has_escapes() {
            Cow::Owned(self.decoded().collect())
        } else {
            Cow::Borrowed(self.contents())
        }
    }

    /// Returns the JSON-encoded contents of the string value. This does not
    /// include the wrapping quotation marks.
    #[must_use]
    pub fn contents(&self) -> &str {
        self.source.as_ref()
    }

    /// Returns the string after decoding any JSON escape sequences.
    #[must_use]
    pub fn decoded(&self) -> Decoded<'_> {
        Decoded::new(self.contents())
    }
}

impl<'a> From<&'a str> for JsonString<'a> {
    fn from(value: &'a str) -> Self {
        let mut escaped = Escaped::from(value);
        Self {
            source: (&mut escaped).collect(),
            info: escaped.info,
        }
    }
}

impl<'a, 'b> PartialEq<&'a str> for &'b JsonString<'b> {
    fn eq(&self, other: &&'a str) -> bool {
        (*self) == other
    }
}

impl<'a, 'b> PartialEq<&'a str> for JsonString<'b> {
    fn eq(&self, other: &&'a str) -> bool {
        let unescaped_length = self.info.unescaped_length();
        let source = self.source.as_ref();
        if self.info.has_escapes() {
            // Quick check, if the decoded length differs, the strings can't be equal
            if unescaped_length != other.len() {
                return false;
            }

            Decoded::new(source).zip(other.chars()).all(|(a, b)| a == b)
        } else {
            // Direct string comparison excluding the quotes.
            source == *other
        }
    }
}

#[test]
fn json_string_from_json() {
    assert_eq!(
        JsonString::from_json(r#""Hello, World!""#).unwrap(),
        JsonString {
            source: Cow::Borrowed(r#"Hello, World!"#),
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
    fn test_json_ne(json: &str, expected: &str) {
        assert_ne!(JsonString::from_json(json).unwrap(), expected);
    }

    test_json(r#""Hello, World!""#, "Hello, World!");
    test_json(r#""\"\\\/\b\f\n\r\t\u25eF""#, "\"\\/\x07\x0c\n\r\t\u{25ef}");
    test_json("\"\u{25ef}\"", "\u{25ef}");
    // Test decoded length not being the same
    test_json_ne(r#""\n""#, "");
    // Test decoded char not matching
    test_json_ne(r#""\n""#, "x");
    // Test regular char not matching in decoded comparison
    test_json_ne(r#""\na""#, "\nx");
}

#[test]
fn decode_if_needed() {
    let empty = JsonString::from_json(r#""""#).unwrap();
    let Cow::Borrowed(string) = empty.decode_if_needed() else { unreachable!() };
    assert_eq!(string, "");
    let has_escapes = JsonString::from_json(r#""\r""#).unwrap();
    let Cow::Owned(string) = has_escapes.decode_if_needed() else { unreachable!() };
    assert_eq!(string, "\r");
}

/// Information about a [`JsonString`]:
///
/// - Whether any escape sequences are in the source
/// - The length of the String if the escape sequences are decoded.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct JsonStringInfo(usize);

impl JsonStringInfo {
    const HAS_ESCAPES_MASK: usize = usize::MAX ^ Self::UNESCAPED_BYTES_MASK;
    /// Information with no escapes and 0 length.
    pub const NONE: Self = Self::new(false, 0);
    #[cfg(target_pointer_width = "32")]
    const UNESCAPED_BYTES_MASK: usize = 0x7FFF_FFFF;
    #[cfg(target_pointer_width = "64")]
    const UNESCAPED_BYTES_MASK: usize = 0x7FFF_FFFF_FFFF_FFFF;

    /// Returns a new instance with the initial values provided.
    #[must_use]
    pub const fn new(has_escapes: bool, unescaped_length: usize) -> Self {
        // Panics aren't allowed in const fns yet.
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

#[test]
fn test_string_info_debug() {
    let mut info = JsonStringInfo::NONE;
    info.add_bytes_from_escape(1);
    assert_eq!(
        format!("{info:?}"),
        "EscapeInfo { has_escapes: true, unescaped_length: 1 }"
    );
}

pub struct Decoded<'a> {
    source: &'a str,
    chars: CharIndices<'a>,
}

impl<'a> Decoded<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices(),
        }
    }
}

impl<'a> Iterator for Decoded<'a> {
    type Item = char;

    #[allow(unsafe_code)]
    fn next(&mut self) -> Option<Self::Item> {
        let (_, ch) = self.chars.next()?;
        if ch == '\\' {
            match self.chars.next().expect("already validated") {
                (_, 'r') => Some('\r'),
                (_, 'n') => Some('\n'),
                (_, 't') => Some('\t'),
                (_, 'b') => Some('\x07'),
                (_, 'f') => Some('\x0c'),
                (offset, 'u') => {
                    // four hex digits
                    for _ in 0..4 {
                        self.chars.next();
                    }
                    // We access the string slice directly rather than
                    // trying to rebuild via char indicies for speed.
                    let hex = &self.source[offset + 1..offset + 5];
                    let value = u16::from_str_radix(hex, 16).expect("already validated");

                    // SAFETY: The JSON string has already had its UTF
                    // escapes validated.
                    Some(unsafe { char::from_u32_unchecked(u32::from(value)) })
                }
                (_, other) => Some(other),
            }
        } else {
            Some(ch)
        }
    }
}

pub struct Escaped<'a> {
    chars: Chars<'a>,
    state: EscapeState,
    info: JsonStringInfo,
}

enum EscapeState {
    None,
    Single(u8),
    Unicode {
        current_nibble: Option<u8>,
        codepoint: u16,
    },
}

impl<'a> From<&'a str> for Escaped<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            chars: value.chars(),
            state: EscapeState::None,
            info: JsonStringInfo::NONE,
        }
    }
}

impl<'a> Iterator for Escaped<'a> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            EscapeState::None => {
                let ch = self.chars.next()?;
                match u8::try_from(ch) {
                    Ok(b) if SAFE_STRING_BYTES[b as usize] => {
                        self.info.add_bytes(1);
                        Some(ch)
                    }
                    Ok(b'"' | b'\\' | b'/') => {
                        self.state = EscapeState::Single(ch as u8);
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Ok(b'\x07') => {
                        self.state = EscapeState::Single(b'b');
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Ok(b'\n') => {
                        self.state = EscapeState::Single(b'n');
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Ok(b'\r') => {
                        self.state = EscapeState::Single(b'r');
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Ok(b'\t') => {
                        self.state = EscapeState::Single(b't');
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Ok(b'\x0c') => {
                        self.state = EscapeState::Single(b'f');
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Ok(b) if (0..=31).contains(&b) => {
                        // Another control character, but it's not a common one.
                        // This must be encoded as a unicode escape.
                        self.state = EscapeState::Unicode {
                            current_nibble: None,
                            codepoint: u16::from(b),
                        };
                        self.info.add_bytes_from_escape(1);
                        Some('\\')
                    }
                    Err(_) => {
                        // A unicode character
                        self.info.add_bytes(ch.len_utf8());
                        Some(ch)
                    }
                    _ => unreachable!("SAFE_CHARACTERS prevents these"),
                }
            }
            EscapeState::Single(ch) => {
                let ch = *ch;
                self.state = EscapeState::None;
                Some(ch as char)
            }
            EscapeState::Unicode {
                current_nibble,
                codepoint,
            } => match current_nibble {
                Some(current_nibble) => {
                    #[allow(clippy::cast_possible_truncation)]
                    let bits = *codepoint as u8 & 0xF;
                    *codepoint >>= 4;
                    let ch = if bits < 10 { bits + b'0' } else { bits + b'a' };
                    *current_nibble += 1;
                    if *current_nibble == 4 {
                        self.state = EscapeState::None;
                    }
                    Some(ch as char)
                }
                None => {
                    *current_nibble = Some(0);
                    Some('u')
                }
            },
        }
    }
}

#[allow(clippy::inconsistent_digit_grouping)]
pub(crate) static HEX_OFFSET_TABLE: [u8; 256] = {
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

pub(crate) static SAFE_STRING_BYTES: &[bool; 256] = {
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

pub(crate) static SAFE_STRING_BYTES_VERIFY_UTF8: &[bool; 256] = {
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

#[test]
fn escape() {
    let original = "\"\\/\u{07}\t\n\r\u{0c}\u{25ef}";
    let json = JsonString::from(original);
    assert!(json.info.has_escapes());
    assert_eq!(json.info.unescaped_length(), original.len());
    assert_eq!(json, original);
}
