use crate::cow::CowStr;
use crate::parser::{ParseDelegate, Parser};
use crate::{Error, ErrorKind};

/// A JSON-encoded number.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct JsonNumber<'a> {
    /// The JSON source for this number.
    pub(crate) source: CowStr<'a>,
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

    /// Parses the contained value as an [`f64`], if possible.
    ///
    /// The JSON parser only validates that the number takes a correct form. If
    /// a number cannot be parsed by the underlying routine due to having too
    /// many digits, it this function can return None.
    #[must_use]
    pub fn as_f64(&self) -> Option<f64> {
        self.source().parse().ok()
    }

    /// Parses the contained value as an [`i64`], if possible.
    ///
    /// If the source number is a floating point number, this will always return None.
    #[must_use]
    pub fn as_i64(&self) -> Option<i64> {
        self.source().parse().ok()
    }

    /// Parses the contained value as an [`u64`], if possible.
    ///
    /// If the source number is a floating point number or has a negative sign,
    /// this will always return None.
    #[must_use]
    pub fn as_u64(&self) -> Option<u64> {
        self.source().parse().ok()
    }
}

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
        unreachable!()
    }

    fn object_value(
        &mut self,
        _object: &mut Self::Object,
        _key: Self::Key,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        unreachable!()
    }

    fn object_is_empty(&self, _object: &Self::Object) -> bool {
        unreachable!()
    }

    fn end_object(&mut self, _object: Self::Object) -> Result<Self::Value, Self::Error> {
        unreachable!()
    }

    fn begin_array(&mut self) -> Result<Self::Array, Self::Error> {
        Err(ErrorKind::ExpectedNumber)
    }

    fn array_value(
        &mut self,
        _array: &mut Self::Array,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        unreachable!()
    }

    fn array_is_empty(&self, _array: &Self::Array) -> bool {
        unreachable!()
    }

    fn end_array(&mut self, _array: Self::Array) -> Result<Self::Value, Self::Error> {
        unreachable!()
    }

    fn kind_of(&self, _value: &Self::Value) -> crate::parser::JsonKind {
        unreachable!()
    }
}

#[test]
#[cfg(feature = "alloc")]
fn json_number_from_json() {
    assert_eq!(
        JsonNumber::from_json("1").unwrap(),
        JsonNumber {
            source: CowStr::Borrowed("1")
        }
    );

    let expected_number = JsonNumber::from_json(r#"true"#)
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
