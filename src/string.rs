use std::borrow::Cow;
use std::fmt::{Display, Write};
use std::str::{CharIndices, Chars};

use crate::error::{Error, ErrorKind};
use crate::value::Value;

/// A JSON-encoded string.
// TODO document comparisons
#[derive(Debug, Eq, Clone)]

pub struct JsonString<'a> {
    /// The JSON-source for the string.
    pub(crate) source: StringContents<'a>,
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
        match &self.source {
            StringContents::Json { source, info } => {
                if info.has_escapes() {
                    Cow::Owned(self.decoded().collect())
                } else {
                    source.clone()
                }
            }
            StringContents::Raw(raw) => raw.clone(),
        }
    }

    /// Returns the string after decoding any JSON escape sequences.
    #[must_use]
    pub fn decoded(&self) -> Decoded<'_> {
        Decoded::new(&self.source)
    }

    /// Returns the string after decoding any JSON escape sequences.
    #[must_use]
    pub fn as_json(&self) -> AsJson<'_> {
        AsJson::from(&self.source)
    }
}

impl<'a> From<&'a str> for JsonString<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            source: StringContents::Raw(Cow::Borrowed(value)),
        }
    }
}

impl<'a> From<String> for JsonString<'a> {
    fn from(value: String) -> Self {
        Self {
            source: StringContents::Raw(Cow::Owned(value)),
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
        match &self.source {
            StringContents::Json { source, info } => {
                let unescaped_length = info.unescaped_length();
                if info.has_escapes() {
                    // Quick check, if the decoded length differs, the strings can't be equal
                    if unescaped_length != other.len() {
                        return false;
                    }

                    Decoded::new(&self.source)
                        .zip(other.chars())
                        .all(|(a, b)| a == b)
                } else {
                    // Direct string comparison excluding the quotes.
                    source == *other
                }
            }
            StringContents::Raw(source) => source == other,
        }
    }
}

impl<'a, 'b> PartialEq<JsonString<'a>> for JsonString<'b> {
    fn eq(&self, other: &JsonString<'a>) -> bool {
        match (&self.source, &other.source) {
            (
                StringContents::Json {
                    source: a,
                    info: a_info,
                },
                StringContents::Json {
                    source: b,
                    info: b_info,
                },
            ) => match (a_info.has_escapes(), b_info.has_escapes()) {
                (true, true) => {
                    if a_info.unescaped_length() == b_info.unescaped_length() {
                        Decoded::new(&self.source)
                            .zip(Decoded::new(&other.source))
                            .all(|(a, b)| a == b)
                    } else {
                        false
                    }
                }
                (true, false) => self == b.as_ref(),
                (false, true) => other == a.as_ref(),
                (false, false) => a.as_ref() == b.as_ref(),
            },
            (StringContents::Raw(a), StringContents::Raw(b)) => a.as_ref() == b.as_ref(),
            (StringContents::Json { .. }, StringContents::Raw(b)) => self == b.as_ref(),
            (StringContents::Raw(a), _) => other == a.as_ref(),
        }
    }
}

#[test]
fn json_string_from_json() {
    assert_eq!(
        JsonString::from_json(r#""Hello, World!""#).unwrap(),
        JsonString {
            source: StringContents::Json {
                source: Cow::Borrowed(r#"Hello, World!"#),
                info: JsonStringInfo::new(false, 13),
            }
        }
    );

    let expected_string = JsonString::from_json(r#"true"#)
        .expect_err("shouldn't allow non-strings")
        .kind;
    assert!(matches!(expected_string, ErrorKind::ExpectedString));
}

#[test]
fn json_string_cmp() {
    #[track_caller]
    fn test_json(json: &str, expected: &str) {
        assert_eq!(JsonString::from_json(json).unwrap(), expected);
    }
    #[track_caller]
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
    needs_decoding: bool,
    source: &'a str,
    chars: CharIndices<'a>,
}

impl<'a> Clone for Decoded<'a> {
    fn clone(&self) -> Self {
        Self {
            needs_decoding: self.needs_decoding,
            source: self.source,
            chars: self.chars.clone(),
        }
    }
}

impl<'a> Decoded<'a> {
    fn new(source: &'a StringContents<'a>) -> Self {
        match source {
            StringContents::Json { source, info } => Self {
                needs_decoding: info.has_escapes(),
                source,
                chars: source.char_indices(),
            },
            StringContents::Raw(source) => Self {
                needs_decoding: false,
                source,
                chars: source.char_indices(),
            },
        }
    }
}

impl<'a> Iterator for Decoded<'a> {
    type Item = char;

    #[allow(unsafe_code)]
    fn next(&mut self) -> Option<Self::Item> {
        let (_, ch) = self.chars.next()?;
        if self.needs_decoding && ch == '\\' {
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
                    let mut codepoint = u32::from_str_radix(hex, 16).expect("already validated");

                    if (HIGH_SURROGATE_MIN..=HIGH_SURROGATE_MAX).contains(&codepoint) {
                        // Surrogate pair, which has already been validated.
                        // Process another \uxxxx.
                        for _ in 0..6 {
                            self.chars.next();
                        }
                        let hex = &self.source[offset + 7..offset + 11];
                        let second_codepoint =
                            u32::from_str_radix(hex, 16).expect("already validated");
                        codepoint = merge_surrogate_pair(codepoint, second_codepoint);
                    }

                    // SAFETY: The JSON string has already had its UTF
                    // escapes validated.
                    Some(unsafe { char::from_u32_unchecked(codepoint) })
                }
                (_, other) => Some(other),
            }
        } else {
            Some(ch)
        }
    }
}

impl<'a> Display for Decoded<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ch in self.clone() {
            f.write_char(ch)?;
        }

        Ok(())
    }
}

pub struct AsJson<'a> {
    chars: Chars<'a>,
    state: EscapeState,
    info: JsonStringInfo,
}

enum EscapeState {
    AlreadyEscaped,
    None,
    Single(u8),
    Unicode {
        current_nibble: Option<u8>,
        codepoint: u16,
    },
}

impl<'b, 'a> From<&'b StringContents<'a>> for AsJson<'b> {
    fn from(value: &'b StringContents<'a>) -> Self {
        match value {
            StringContents::Json { source, info } => Self {
                chars: source.chars(),
                state: EscapeState::AlreadyEscaped,
                info: *info,
            },
            StringContents::Raw(source) => Self {
                chars: source.chars(),
                state: EscapeState::None,
                info: JsonStringInfo::NONE,
            },
        }
    }
}

impl<'a> Iterator for AsJson<'a> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            EscapeState::AlreadyEscaped => self.chars.next(),
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

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.chars.size_hint()
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
    let raw = JsonString::from(original);
    let decoded = raw.decoded().collect::<String>();
    assert_eq!(decoded, original);
    let json = raw.as_json().collect::<String>();
    assert_eq!(json, "\\\"\\\\/\\b\\t\\n\\r\\f\u{25ef}");
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StringContents<'a> {
    Json {
        source: Cow<'a, str>,
        info: JsonStringInfo,
    },
    Raw(Cow<'a, str>),
}

pub(crate) const HIGH_SURROGATE_MIN: u32 = 0xD800;
pub(crate) const HIGH_SURROGATE_MAX: u32 = 0xDBFF;
pub(crate) const LOW_SURROGATE_MIN: u32 = 0xDC00;
pub(crate) const LOW_SURROGATE_MAX: u32 = 0xDFFF;
pub(crate) const DECODED_SURROGATE_BASE: u32 = 0x1_0000;

#[inline]
pub(crate) fn merge_surrogate_pair(high: u32, low: u32) -> u32 {
    DECODED_SURROGATE_BASE | ((high - HIGH_SURROGATE_MIN) * 0x400) | (low - LOW_SURROGATE_MIN)
}
