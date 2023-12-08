use core::str::FromStr;

use crate::anystr::AnyStr;
use crate::parser::{ParseDelegate, Parser};
use crate::{Error, ErrorKind};

/// A JSON-encoded number.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct JsonNumber<'a> {
    /// The JSON source for this number.
    pub(crate) source: AnyStr<'a>,
}

impl<'a> JsonNumber<'a> {
    /// Parses `json`, expecting a single number value.
    ///
    /// # Errors
    ///
    /// Returns [`ErrorKind::ExpectedString`] if a non-string value is
    /// encountered.
    pub fn from_json(json: &'a str) -> Result<Self, Error> {
        Parser::parse_json(json, NumberParser).map_err(Error::into_infallable)
    }

    /// Returns the JSON-encoded representation of this number.
    #[must_use]
    pub fn source(&self) -> &str {
        self.source.as_ref()
    }

    /// Parses the contained value as an [`f32`], if possible.
    ///
    /// The JSON parser only validates that the number takes a correct form. If
    /// a number cannot be parsed by the underlying routine due to having too
    /// many digits, it this function can return None.
    #[must_use]
    pub fn as_f32(&self) -> Option<f32> {
        self.parse().ok()
    }

    /// Parses the contained value as an [`f64`], if possible.
    ///
    /// The JSON parser only validates that the number takes a correct form. If
    /// a number cannot be parsed by the underlying routine due to having too
    /// many digits, it this function can return None.
    #[must_use]
    pub fn as_f64(&self) -> Option<f64> {
        self.parse().ok()
    }

    /// Parses the contained value.
    pub fn parse<T: FromStr>(&self) -> Result<T, T::Err> {
        self.source().parse()
    }
}

macro_rules! impl_as_number {
    ($name:ident, $type:ident) => {
        impl JsonNumber<'_> {
            /// Parses the contained value as an
            #[doc = concat!("[`", stringify!($type), "`]")]
            /// if possible.
            ///
            /// If the source number is a floating point number or has a negative sign,
            /// this will always return None.
            #[must_use]
            pub fn $name(&self) -> Option<$type> {
                self.parse().ok()
            }
        }
    };
}

impl_as_number!(as_u8, u8);
impl_as_number!(as_u16, u16);
impl_as_number!(as_u32, u32);
impl_as_number!(as_u64, u64);
impl_as_number!(as_u128, u128);
impl_as_number!(as_usize, usize);
impl_as_number!(as_i8, i8);
impl_as_number!(as_i16, i16);
impl_as_number!(as_i32, i32);
impl_as_number!(as_i64, i64);
impl_as_number!(as_i128, i128);
impl_as_number!(as_isize, isize);

struct NumberParser;

impl<'a> ParseDelegate<'a> for NumberParser {
    type Array = ();
    type Error = ErrorKind;
    type Key = ();
    type Object = ();
    type Value = JsonNumber<'a>;

    fn null(&mut self) -> Result<Self::Value, Self::Error> {
        Err(ErrorKind::ExpectedNumber)
    }

    fn boolean(&mut self, _value: bool) -> Result<Self::Value, Self::Error> {
        Err(ErrorKind::ExpectedNumber)
    }

    fn number(&mut self, value: JsonNumber<'a>) -> Result<Self::Value, Self::Error> {
        Ok(value)
    }

    fn string(&mut self, _value: crate::JsonString<'a>) -> Result<Self::Value, Self::Error> {
        Err(ErrorKind::ExpectedNumber)
    }

    fn begin_object(&mut self) -> Result<Self::Object, Self::Error> {
        Err(ErrorKind::ExpectedNumber)
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
        Err(ErrorKind::ExpectedNumber)
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
#[cfg(feature = "alloc")]
fn json_number_from_json() {
    assert_eq!(
        JsonNumber::from_json("1").unwrap(),
        JsonNumber {
            source: AnyStr::Borrowed("1")
        }
    );

    let expected_number = JsonNumber::from_json("true")
        .expect_err("shouldn't allow non-numbers")
        .kind;
    assert!(matches!(expected_number, ErrorKind::ExpectedNumber));
}

#[test]
#[cfg(feature = "alloc")]
fn json_number_conversions() {
    let one = JsonNumber::from_json("1").unwrap();
    assert_eq!(one.as_i64().unwrap(), 1);
    assert_eq!(one.as_u64().unwrap(), 1);
    assert!((one.as_f64().unwrap() - 1.0).abs() < f64::EPSILON);
}

#[test]
fn from_json_bad_types() {
    assert_eq!(
        JsonNumber::from_json("\"\"").unwrap_err().kind,
        ErrorKind::ExpectedNumber
    );
    assert_eq!(
        JsonNumber::from_json("null").unwrap_err().kind,
        ErrorKind::ExpectedNumber
    );
    assert_eq!(
        JsonNumber::from_json("true").unwrap_err().kind,
        ErrorKind::ExpectedNumber
    );
    assert_eq!(
        JsonNumber::from_json("[]").unwrap_err().kind,
        ErrorKind::ExpectedNumber
    );
    assert_eq!(
        JsonNumber::from_json("{}").unwrap_err().kind,
        ErrorKind::ExpectedNumber
    );
}

#[test]
#[cfg(feature = "alloc")]
fn as_es() {
    macro_rules! test_as {
        ($as:ident) => {
            assert_eq!(JsonNumber::from_json("1").unwrap().$as(), Some(1));
        };
    }

    test_as!(as_i8);
    test_as!(as_i16);
    test_as!(as_i32);
    test_as!(as_i64);
    test_as!(as_i128);
    test_as!(as_isize);
    test_as!(as_u8);
    test_as!(as_u16);
    test_as!(as_u32);
    test_as!(as_u64);
    test_as!(as_u128);
    test_as!(as_usize);

    assert!(JsonNumber::from_json("0").unwrap().as_f32().unwrap().abs() < f32::EPSILON);
    assert!(JsonNumber::from_json("0").unwrap().as_f64().unwrap().abs() < f64::EPSILON);
}
