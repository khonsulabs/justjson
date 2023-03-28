#[cfg(feature = "alloc")]
use alloc::string::String;
use core::cmp::Ordering;
use core::fmt::{self, Debug, Display, Formatter, Write};
use core::hash;
use core::str::{CharIndices, Chars};

use crate::anystr::AnyStr;
use crate::error::{Error, ErrorKind};
use crate::parser::{ParseDelegate, Parser};

/// A JSON string.
///
/// Internally, this type may be represented by either an already escaped JSON
/// string or an unescaped string.
///
/// This type implements `Eq` and `PartialEq` such that the escapes are always
/// handled correctly. Consider the following examples:
///
/// ```rust
/// use justjson::JsonString;
///
/// let json = JsonString::from_json(r#""Check out this cat: \"\ud83d\ude39\"""#).unwrap();
/// assert_eq!(json, "Check out this cat: \"😹\"");
/// let alternate_form =
///     JsonString::from_json("\"Check out this cat: \\\"\u{1f639}\\\"\"").unwrap();
/// assert_eq!(json, alternate_form)
/// ```
///
/// When the underlying representation is an encoded JSON string, the
/// [`JsonStringInfo`] is stored from the parsing operation. This allows the
/// comparison implementations to perform quick length checks on the decoded
/// length to avoid comparing the bytes of strings that decode to different
/// lengths.
///
/// Additionally, this type implements `Ord`, `PartialOrd`, and `Hash` in ways
/// that are consistent regardless of whether the string has escape sequences or
/// not. If the underlying representation does not need extra processing, the
/// built-in implementations for `&str` are always used.
#[derive(Debug, Eq, Clone)]
pub struct JsonString<'a> {
    /// The JSON-source for the string.
    pub(crate) source: StringContents<'a>,
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
        Parser::parse_json(json, StringParser).map_err(Error::into_infallable)
    }

    /// Returns the contained string after decoding any escape sequences, if
    /// needed. If there are no escape sequences, the contents of the string are
    /// borrowed.
    ///
    /// This type keeps track of whether the underlying string contains escape
    /// sequences, so this function is nearly instananeous when there are no
    /// escape sequences.
    #[must_use]
    #[cfg(feature = "alloc")]
    pub fn decode_if_needed(&self) -> AnyStr<'a> {
        match &self.source {
            StringContents::Json(source) => {
                if self.info.has_escapes() {
                    AnyStr::Owned(self.decoded().collect())
                } else {
                    source.clone()
                }
            }
            StringContents::Raw(raw) => raw.clone(),
        }
    }

    /// Returns the contained string after escaping any characters, if needed.
    /// If the string is already internally represented as a JSON string, this
    /// function returns its source via borrowing and no additional processing.
    #[must_use]
    #[cfg(feature = "alloc")]
    pub fn escape_if_needed(&self) -> AnyStr<'a> {
        match &self.source {
            StringContents::Json(source) => source.clone(),
            StringContents::Raw(_) => {
                // We promote raw strings with no escapes to Json strings, which
                // means we will always need to escape this raw string.
                let mut json = String::with_capacity(self.info.expected_length());
                json.extend(AsJson::new(&self.source, self.info));
                AnyStr::Owned(json)
            }
        }
    }

    /// Returns the string after decoding any JSON escape sequences.
    #[must_use]
    pub fn decoded(&self) -> Decoded<'_> {
        Decoded::new(&self.source, self.info)
    }

    /// Returns the string after decoding any JSON escape sequences.
    #[must_use]
    pub fn as_json(&self) -> AsJson<'_> {
        AsJson::new(&self.source, self.info)
    }

    /// Returns a reference to the underlying storage of this string, if it's
    /// already encoded for use in a JSON string. This does not include the
    /// surrounding quotation marks.
    ///
    /// If None is returned, the string must be decoded using
    /// [`as_json()`](Self::as_json).
    #[must_use]
    pub fn as_json_str(&self) -> Option<&'_ AnyStr<'a>> {
        if let StringContents::Json(source) = &self.source {
            Some(source)
        } else {
            None
        }
    }

    /// Returns the length of this string when encoded as JSON.
    #[must_use]
    pub fn len(&self) -> usize {
        match &self.source {
            StringContents::Json(json) => json.len(),
            StringContents::Raw(_) => self.info.expected_length(),
        }
    }

    /// Returns true if this string is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the length of this string when decoded.
    #[must_use]
    pub fn decoded_len(&self) -> usize {
        match &self.source {
            StringContents::Json(_) => self.info.expected_length(),
            StringContents::Raw(raw) => raw.len(),
        }
    }

    /// Returns a reference to the contents of this value if the contained
    /// string does not have any escape sequences that must be decoded.
    #[must_use]
    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        match &self.source {
            StringContents::Json(str) if !self.info.has_escapes() => Some(str),
            StringContents::Json(_) => None,
            StringContents::Raw(str) => Some(str),
        }
    }
}

impl<'a> From<&'a str> for JsonString<'a> {
    fn from(value: &'a str) -> Self {
        // Analyze the string for characters that need to be escaped.
        let source = StringContents::Raw(AnyStr::Borrowed(value));
        let mut escaped = AsJson::new(&source, JsonStringInfo::NONE);
        for _ in &mut escaped {}

        if escaped.info.has_escapes() {
            Self {
                info: escaped.info,
                source,
            }
        } else {
            // No escapes, we can promote to a JSON string.
            Self {
                source: StringContents::Json(AnyStr::Borrowed(value)),
                info: escaped.info,
            }
        }
    }
}

#[cfg(feature = "alloc")]
impl<'a> From<String> for JsonString<'a> {
    fn from(value: String) -> Self {
        // Analyze the string for characters that need to be escaped.
        let borrowed_contents = StringContents::Raw(AnyStr::Borrowed(&value));
        let mut escaped = AsJson::new(&borrowed_contents, JsonStringInfo::NONE);
        for _ in &mut escaped {}

        if escaped.info.has_escapes() {
            Self {
                info: escaped.info,
                source: StringContents::Raw(AnyStr::Owned(value)),
            }
        } else {
            // No escapes, we can promote to a JSON string.
            Self {
                info: escaped.info,
                source: StringContents::Json(AnyStr::Owned(value)),
            }
        }
    }
}

impl<'a, 'b> PartialEq<&'a str> for JsonString<'b> {
    fn eq(&self, other: &&'a str) -> bool {
        self.eq(*other)
    }
}

impl<'b> PartialEq<str> for JsonString<'b> {
    fn eq(&self, other: &str) -> bool {
        match &self.source {
            StringContents::Json(source) => {
                if self.info.has_escapes() {
                    // Quick check, if the decoded length differs, the strings can't be equal
                    if self.info.expected_length() != other.len() {
                        return false;
                    }

                    Decoded::new(&self.source, self.info)
                        .zip(other.chars())
                        .all(|(a, b)| a == b)
                } else {
                    // Direct string comparison excluding the quotes.
                    source == other
                }
            }
            StringContents::Raw(source) => source == other,
        }
    }
}

impl<'a, 'b> PartialEq<JsonString<'a>> for JsonString<'b> {
    fn eq(&self, other: &JsonString<'a>) -> bool {
        match (&self.source, &other.source) {
            (StringContents::Json(a), StringContents::Json(b)) => {
                match (self.info.has_escapes(), other.info.has_escapes()) {
                    (true, true) => {
                        if self.info.expected_length() == other.info.expected_length() {
                            Decoded::new(&self.source, self.info)
                                .zip(Decoded::new(&other.source, self.info))
                                .all(|(a, b)| a == b)
                        } else {
                            false
                        }
                    }
                    (true, false) => self == b.as_ref(),
                    (false, true) => other == a.as_ref(),
                    (false, false) => a.as_ref() == b.as_ref(),
                }
            }
            (StringContents::Raw(a), StringContents::Raw(b)) => a.as_ref() == b.as_ref(),
            (StringContents::Json(_), StringContents::Raw(b)) => {
                if other.info.expected_length() == self.len() {
                    self == b.as_ref()
                } else {
                    false
                }
            }
            (StringContents::Raw(a), _) => {
                if self.info.expected_length() == other.len() {
                    other == a.as_ref()
                } else {
                    false
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "alloc")]
fn json_string_eq() {
    #[track_caller]
    fn test_json<'a, T>(json: &'a str, expected: T)
    where
        T: Debug,
        JsonString<'a>: PartialEq<T>,
    {
        assert_eq!(JsonString::from_json(json).unwrap(), expected);
    }

    #[track_caller]
    fn test_json_ne<'a, T>(json: &'a str, expected: T)
    where
        T: Debug,
        JsonString<'a>: PartialEq<T>,
    {
        assert_ne!(JsonString::from_json(json).unwrap(), expected);
    }

    // Test &str comparisons
    test_json(r#""Hello, World!""#, "Hello, World!");
    test_json(r#""\"\\\/\b\f\n\r\t\u25eF""#, "\"\\/\x07\x0c\n\r\t\u{25ef}");
    test_json("\"\u{25ef}\"", "\u{25ef}");
    // Test decoded length not being the same
    test_json_ne(r#""\n""#, "");
    // Test decoded char not matching
    test_json_ne(r#""\n""#, "x");
    // Test regular char not matching in decoded comparison
    test_json_ne(r#""\na""#, "\nx");

    // Test JsonString comparisons
    // Decoded length not being the same between two JsonStrings.
    test_json_ne(
        r#""\u0000""#,
        JsonString::from_json(r#""\u0000\u0000""#).unwrap(),
    );
    // Same decoded length, lhs doesn't have escapes, rhs does
    test_json_ne(r#""ab""#, JsonString::from_json(r#""\u0000""#).unwrap());
    // Raw strings in JsonString
    assert_eq!(JsonString::from("a"), "a");
    assert_eq!(JsonString::from("a"), JsonString::from("a"));
    assert_eq!(
        JsonString::from_json("\"a\"").unwrap(),
        JsonString::from("a")
    );
    assert_eq!(
        JsonString::from("a"),
        JsonString::from_json("\"a\"").unwrap()
    );

    assert_eq!(JsonString::from("\0"), "\0");
    assert_eq!(JsonString::from("\0"), JsonString::from("\0"));

    assert_eq!(
        JsonString::from_json(r#""\u0000""#).unwrap(),
        JsonString::from("\0"),
    );
    assert_eq!(
        JsonString::from("\0"),
        JsonString::from_json(r#""\u0000""#).unwrap(),
    );

    assert_ne!(
        JsonString::from_json(r#""\u0000""#).unwrap(),
        JsonString::from("\0 "),
    );
    assert_ne!(
        JsonString::from("\0 "),
        JsonString::from_json(r#""\u0000""#).unwrap(),
    );
}

impl<'b> PartialOrd<str> for JsonString<'b> {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        match &self.source {
            StringContents::Json(_) if self.info.has_escapes() => {
                let mut left_chars = self.decoded();
                let mut right_chars = other.chars();
                loop {
                    match (left_chars.next(), right_chars.next()) {
                        (Some(left), Some(right)) => match left.cmp(&right) {
                            Ordering::Equal => continue,
                            Ordering::Less => return Some(Ordering::Less),
                            Ordering::Greater => return Some(Ordering::Greater),
                        },
                        (Some(_), _) => return Some(Ordering::Greater),
                        (_, Some(_)) => return Some(Ordering::Less),
                        (None, None) => return Some(Ordering::Equal),
                    }
                }
            }
            StringContents::Json(left) | StringContents::Raw(left) => Some(left.cmp(other)),
        }
    }
}

impl<'a, 'b> PartialOrd<&'a str> for JsonString<'b> {
    fn partial_cmp(&self, other: &&'a str) -> Option<Ordering> {
        self.partial_cmp(*other)
    }
}

impl<'b> Ord for JsonString<'b> {
    fn cmp(&self, other: &Self) -> Ordering {
        // There aren't any shortcuts for comparisons
        match (&self.source, &other.source) {
            (StringContents::Json(left), StringContents::Json(right))
                if !self.info.has_escapes() && !other.info.has_escapes() =>
            {
                left.cmp(right)
            }
            (StringContents::Raw(left), StringContents::Raw(right)) => left.cmp(right),
            (StringContents::Json(_), StringContents::Raw(right)) => {
                self.partial_cmp(&&**right).expect("always some")
            }
            (StringContents::Raw(left), StringContents::Json(_)) => {
                other.partial_cmp(&&**left).expect("always some").reverse()
            }
            _ => {
                let mut left_chars = self.decoded();
                let mut right_chars = other.decoded();
                loop {
                    match (left_chars.next(), right_chars.next()) {
                        (Some(left), Some(right)) => match left.cmp(&right) {
                            Ordering::Equal => continue,
                            Ordering::Less => return Ordering::Less,
                            Ordering::Greater => return Ordering::Greater,
                        },
                        (Some(_), _) => return Ordering::Greater,
                        (_, Some(_)) => return Ordering::Less,
                        (None, None) => return Ordering::Equal,
                    }
                }
            }
        }
    }
}
impl<'a, 'b> PartialOrd<JsonString<'a>> for JsonString<'b> {
    fn partial_cmp(&self, other: &JsonString<'a>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(feature = "alloc")]
#[cfg(test)]
macro_rules! jstr {
    ($src:literal) => {
        JsonString::from_json(concat!("\"", $src, "\"")).unwrap()
    };
}

#[test]
#[cfg(feature = "alloc")]
#[allow(clippy::cmp_owned)]
fn json_string_cmp() {
    macro_rules! assert_lt {
        ($left:expr, $right:expr) => {
            assert!($left < $right);
        };
    }

    macro_rules! assert_gt {
        ($left:expr, $right:expr) => {
            assert!($left > $right);
        };
    }

    macro_rules! assert_ord_eq {
        ($left:expr, $right:expr) => {
            assert_eq!($left.partial_cmp($right), Some(Ordering::Equal));
        };
    }

    // no escapes, which ends up using str.cmp
    assert_lt!(jstr!("a"), "b");
    assert_gt!(jstr!("b"), "a");
    assert_ord_eq!(jstr!("a"), "a");
    assert_ord_eq!(JsonString::from("\n"), "\n");

    // Escapes vs no escapes. Same as above, but encoding a/b as unicode escapes
    assert_lt!(jstr!(r#"\u0061"#), "b");
    assert_gt!(jstr!(r#"\u0062"#), "a");
    assert_ord_eq!(jstr!(r#"\u0061"#), "a");
    // differing lengths, but matching prefix
    assert_lt!(jstr!(r#"\u0061"#), "aa");
    assert_gt!(jstr!(r#"\u0061a"#), "a");

    // Same suite of tests, but with json strings on the right
    // Escapes vs no escapes. Same as above, but encoding a/b as unicode escapes
    assert_lt!(jstr!(r#"\u0061"#), JsonString::from("b"));
    assert_gt!(jstr!(r#"\u0062"#), JsonString::from("a"));
    assert_ord_eq!(jstr!(r#"\u0061"#), &JsonString::from("a"));
    // differing lengths, but matching prefix
    assert_lt!(jstr!(r#"\u0061"#), JsonString::from("aa"));
    assert_gt!(jstr!(r#"\u0061a"#), JsonString::from("a"));
    // Raw on both sides
    assert_ord_eq!(JsonString::from("\n"), &JsonString::from("\n"));
    // Raw either side
    assert_lt!(jstr!(r#"\n"#), JsonString::from("\na"));
    assert_gt!(JsonString::from("\na"), jstr!(r#"\n"#));
    // JSON no escapes both sides
    assert_lt!(jstr!("a"), JsonString::from("b"));
}

impl<'a> hash::Hash for JsonString<'a> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match &self.source {
            StringContents::Json(_) if self.info.has_escapes() => {
                // The standard string hash writes out as bytes, but char
                // hashing is done by converting to u32. We're purposely
                // re-implementing Hasher::write_str via chars.
                let mut char_bytes = [0; 4];
                for ch in self.decoded() {
                    state.write(ch.encode_utf8(&mut char_bytes).as_bytes());
                }
                state.write_u8(0xff); // from Hasher::write_str
            }
            StringContents::Json(str) | StringContents::Raw(str) => str.hash(state),
        }
    }
}

#[test]
#[cfg(feature = "std")]
fn json_string_hash() {
    use core::hash::{BuildHasher, Hasher};
    use std::collections::hash_map::RandomState;
    fn hash(t: impl hash::Hash, state: &RandomState) -> u64 {
        let mut hasher = state.build_hasher();
        t.hash(&mut hasher);
        hasher.finish()
    }

    let state = RandomState::default();
    assert_eq!(
        hash(JsonString::from("\n"), &state),
        hash(jstr!("\\n"), &state)
    );
    assert_eq!(hash(jstr!("a"), &state), hash("a", &state));
}

impl<'a> Display for JsonString<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.decoded().fmt(f)
    }
}

#[test]
#[cfg(feature = "alloc")]
fn display() {
    use std::string::ToString;
    let json = JsonString::from_json(r#""hello, world!""#).unwrap();
    assert_eq!(json.to_string(), "hello, world!");
}

#[test]
#[cfg(feature = "alloc")]
fn json_string_from_json() {
    assert_eq!(
        JsonString::from_json(r#""Hello, World!""#).unwrap(),
        JsonString {
            source: StringContents::Json(AnyStr::Borrowed(r#"Hello, World!"#)),
            info: JsonStringInfo::new(false, 13),
        }
    );

    let expected_string = JsonString::from_json(r#"true"#)
        .expect_err("shouldn't allow non-strings")
        .kind;
    assert!(matches!(expected_string, ErrorKind::ExpectedString));
}

#[test]
fn as_str() {
    let json_with_escapes = JsonString::from_json(r#""\n""#).unwrap();
    assert_eq!(json_with_escapes.as_str(), None);
    let json_no_escapes = JsonString::from_json(r#""hi""#).unwrap();
    assert_eq!(json_no_escapes.as_str(), Some("hi"));
    let raw = JsonString::from("hi");
    assert_eq!(raw.as_str(), Some("hi"));
}

#[test]
#[cfg(feature = "alloc")]
fn json_string_from_raw() {
    assert_eq!(JsonString::from(String::from("a")), JsonString::from("a"));
}

#[test]
#[cfg(feature = "alloc")]
fn decode_if_needed() {
    let empty = JsonString::from_json(r#""""#).unwrap();
    let AnyStr::Borrowed(string) = empty.decode_if_needed() else { unreachable!() };
    assert_eq!(string, "");
    let has_escapes = JsonString::from_json(r#""\r""#).unwrap();
    let AnyStr::Owned(string) = has_escapes.decode_if_needed() else { unreachable!() };
    assert_eq!(string, "\r");
    let decoded_via_display = alloc::format!("{}", has_escapes.decoded());
    assert_eq!(decoded_via_display, "\r");

    let raw_string = JsonString::from(r#"raw string"#);
    let AnyStr::Borrowed(string) = raw_string.decode_if_needed() else { unreachable!() };
    assert_eq!(string, "raw string");

    let decoded_via_display = alloc::format!("{}", raw_string.decoded());
    assert_eq!(decoded_via_display, "raw string");
}

/// Information about a parsed [`JsonString`].
///
/// This type stores:
///
/// - Whether any escape sequences are in the source
/// - The length of the String if the escape sequences are decoded.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
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
    pub const fn expected_length(self) -> usize {
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

impl fmt::Debug for JsonStringInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("EscapeInfo")
            .field("has_escapes", &self.has_escapes())
            .field("unescaped_length", &self.expected_length())
            .finish()
    }
}

#[test]
#[cfg(feature = "alloc")]
fn test_string_info_debug() {
    let mut info = JsonStringInfo::NONE;
    info.add_bytes_from_escape(1);
    assert_eq!(
        alloc::format!("{info:?}"),
        "EscapeInfo { has_escapes: true, unescaped_length: 1 }"
    );
}

#[derive(Clone)]
pub struct Decoded<'a> {
    needs_decoding: bool,
    source: &'a str,
    chars: CharIndices<'a>,
}

impl<'a> Decoded<'a> {
    fn new(source: &'a StringContents<'a>, info: JsonStringInfo) -> Self {
        match source {
            StringContents::Json(source) => Self {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for ch in self.clone() {
            f.write_char(ch)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct AsJson<'a> {
    chars: Chars<'a>,
    state: EscapeState,
    info: JsonStringInfo,
}

impl<'a> Display for AsJson<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for ch in self.clone() {
            f.write_char(ch)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
enum EscapeState {
    AlreadyEscaped,
    None,
    Single(u8),
    Unicode {
        current_nibble: Option<u8>,
        codepoint: u16,
    },
}

impl<'b, 'a> AsJson<'b> {
    fn new(value: &'b StringContents<'a>, info: JsonStringInfo) -> Self {
        match value {
            StringContents::Json(source) => Self {
                chars: source.chars(),
                state: EscapeState::AlreadyEscaped,
                info,
            },
            StringContents::Raw(source) => Self {
                chars: source.chars(),
                state: EscapeState::None,
                info,
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
                        self.info.add_bytes_from_escape(2);
                        Some('\\')
                    }
                    Ok(b'\x07') => {
                        self.state = EscapeState::Single(b'b');
                        self.info.add_bytes_from_escape(2);
                        Some('\\')
                    }
                    Ok(b'\n') => {
                        self.state = EscapeState::Single(b'n');
                        self.info.add_bytes_from_escape(2);
                        Some('\\')
                    }
                    Ok(b'\r') => {
                        self.state = EscapeState::Single(b'r');
                        self.info.add_bytes_from_escape(2);
                        Some('\\')
                    }
                    Ok(b'\t') => {
                        self.state = EscapeState::Single(b't');
                        self.info.add_bytes_from_escape(2);
                        Some('\\')
                    }
                    Ok(b'\x0c') => {
                        self.state = EscapeState::Single(b'f');
                        self.info.add_bytes_from_escape(2);
                        Some('\\')
                    }
                    Ok(b) if (0..=31).contains(&b) => {
                        // Another control character, but it's not a common one.
                        // This must be encoded as a unicode escape.
                        self.state = EscapeState::Unicode {
                            current_nibble: None,
                            codepoint: u16::from(b),
                        };
                        self.info.add_bytes_from_escape(6);
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
fn is_empty() {
    assert!(JsonString::from("").is_empty());
    assert!(JsonString::from_json("\"\"").unwrap().is_empty());
    assert!(!JsonString::from(" ").is_empty());
    assert!(!JsonString::from_json("\" \"").unwrap().is_empty());
}

#[test]
#[cfg(feature = "alloc")]
fn escape() {
    let original = "\"\\/\u{07}\t\n\r\u{0c}\u{0}\u{25ef}";
    let escaped = "\\\"\\\\/\\b\\t\\n\\r\\f\\u0000\u{25ef}";
    let raw = JsonString::from(original);
    assert_eq!(raw.len(), escaped.len());
    assert_eq!(raw.decoded_len(), original.len());
    assert_eq!(raw, original);
    let decoded = raw.decoded().collect::<String>();
    assert_eq!(decoded, original);
    let decoded = raw.decode_if_needed();
    assert_eq!(decoded, original);
    let json = raw.as_json().collect::<String>();
    assert_eq!(json, escaped);
    let json = raw.escape_if_needed();
    assert_eq!(json, escaped);
    assert!(raw.as_json_str().is_none());

    // Test converting a String that needs encoding.
    let original = "\"\\/\u{07}\t\n\r\u{0c}\u{0}\u{25ef}";
    let escaped = "\\\"\\\\/\\b\\t\\n\\r\\f\\u0000\u{25ef}";
    let raw: JsonString<'static> = JsonString::from(String::from(original));
    assert_eq!(raw.len(), escaped.len());
    assert_eq!(raw.decoded_len(), original.len());
    assert_eq!(raw, original);
    let decoded = raw.decoded().collect::<String>();
    assert_eq!(decoded, original);
    let decoded = raw.decode_if_needed();
    assert_eq!(decoded, original);
    let json = raw.as_json().collect::<String>();
    assert_eq!(json, escaped);
    let json = raw.escape_if_needed();
    assert_eq!(json, escaped);
    assert!(raw.as_json_str().is_none());

    // Test with a raw string that doesn't need encoding
    let original = "hello";
    let raw = JsonString::from("hello");
    assert_eq!(raw.len(), original.len());
    assert_eq!(raw.decoded_len(), original.len());
    assert_eq!(raw, original);
    let decoded = raw.decoded().collect::<String>();
    assert_eq!(decoded, original);
    let decoded = raw.decode_if_needed();
    assert_eq!(decoded, original);
    let json = raw.as_json().collect::<String>();
    assert_eq!(json, original);
    let json = raw.escape_if_needed();
    assert_eq!(json, original);
    assert_eq!(raw.as_json_str().map(AnyStr::as_ref), Some("hello"));
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum StringContents<'a> {
    Json(AnyStr<'a>),
    Raw(AnyStr<'a>),
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
struct StringParser;

impl<'a> ParseDelegate<'a> for StringParser {
    type Array = ();
    type Error = ErrorKind;
    type Key = ();
    type Object = ();
    type Value = JsonString<'a>;

    fn null(&mut self) -> Result<Self::Value, Self::Error> {
        Err(ErrorKind::ExpectedString)
    }

    fn boolean(&mut self, _value: bool) -> Result<Self::Value, Self::Error> {
        Err(ErrorKind::ExpectedString)
    }

    fn number(&mut self, _value: crate::JsonNumber<'a>) -> Result<Self::Value, Self::Error> {
        Err(ErrorKind::ExpectedString)
    }

    fn string(&mut self, value: JsonString<'a>) -> Result<Self::Value, Self::Error> {
        Ok(value)
    }

    fn begin_object(&mut self) -> Result<Self::Object, Self::Error> {
        Err(ErrorKind::ExpectedString)
    }

    fn object_key(
        &mut self,
        _object: &mut Self::Object,
        _key: crate::JsonString<'a>,
    ) -> Result<Self::Key, Self::Error> {
        unreachable!("error returned from begin_object")
    }

    fn object_value(
        &mut self,
        _object: &mut Self::Object,
        _key: Self::Key,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        unreachable!("error returned from begin_object")
    }

    fn object_is_empty(&self, _object: &Self::Object) -> bool {
        unreachable!("error returned from begin_object")
    }

    fn end_object(&mut self, _object: Self::Object) -> Result<Self::Value, Self::Error> {
        unreachable!("error returned from begin_object")
    }

    fn begin_array(&mut self) -> Result<Self::Array, Self::Error> {
        Err(ErrorKind::ExpectedString)
    }

    fn array_value(
        &mut self,
        _array: &mut Self::Array,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        unreachable!("error returned from array_value")
    }

    fn array_is_empty(&self, _array: &Self::Array) -> bool {
        unreachable!("error returned from array_value")
    }

    fn end_array(&mut self, _array: Self::Array) -> Result<Self::Value, Self::Error> {
        unreachable!("error returned from array_value")
    }

    fn kind_of(&self, _value: &Self::Value) -> crate::parser::JsonKind {
        unreachable!("allow_all_types_at_root is always true")
    }
}

#[test]
fn from_json_bad_types() {
    assert_eq!(
        JsonString::from_json("1").unwrap_err().kind,
        ErrorKind::ExpectedString
    );
    assert_eq!(
        JsonString::from_json("null").unwrap_err().kind,
        ErrorKind::ExpectedString
    );
    assert_eq!(
        JsonString::from_json("true").unwrap_err().kind,
        ErrorKind::ExpectedString
    );
    assert_eq!(
        JsonString::from_json("[]").unwrap_err().kind,
        ErrorKind::ExpectedString
    );
    assert_eq!(
        JsonString::from_json("{}").unwrap_err().kind,
        ErrorKind::ExpectedString
    );
}
