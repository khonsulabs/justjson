use crate::{Error, ErrorKind, Value};

/// A JSON-encoded number.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct JsonNumber<Backing> {
    /// The JSON source for this number.
    pub source: Backing,
}

impl<'a> JsonNumber<&'a str> {
    /// Parses `json`, expecting a single number value.
    ///
    /// # Errors
    ///
    /// Returns [`ErrorKind::ExpectedString`] if a non-string value is
    /// encountered.
    pub fn from_json(json: &'a str) -> Result<Self, Error> {
        if let Value::Number(str) = Value::from_json(json)? {
            Ok(str)
        } else {
            Err(Error {
                offset: 0,
                kind: ErrorKind::ExpectedNumber,
            })
        }
    }
}

impl<Backing> JsonNumber<Backing>
where
    Backing: AsRef<str>,
{
    /// Parses the contained value as an [`f64`], if possible.
    ///
    /// The JSON parser only validates that the number takes a correct form. If
    /// a number cannot be parsed by the underlying routine due to having too
    /// many digits, it this function can return None.
    pub fn as_f64(&self) -> Option<f64> {
        self.source.as_ref().parse().ok()
    }

    /// Parses the contained value as an [`i64`], if possible.
    ///
    /// If the source number is a floating point number, this will always return None.
    pub fn as_i64(&self) -> Option<i64> {
        self.source.as_ref().parse().ok()
    }

    /// Parses the contained value as an [`u64`], if possible.
    ///
    /// If the source number is a floating point number or has a negative sign,
    /// this will always return None.
    pub fn as_u64(&self) -> Option<u64> {
        self.source.as_ref().parse().ok()
    }

    // pub fn into_owned(self) -> JsonNumber<'static> {
    //     JsonNumber {
    //         source: Cow::Owned(match self.source {
    //             Cow::Borrowed(value) => value.to_string(),
    //             Cow::Owned(value) => value,
    //         }),
    //     }
    // }

    // pub fn to_owned(&self) -> JsonNumber<'static> {
    //     JsonNumber {
    //         source: Cow::Owned(match &self.source {
    //             Cow::Borrowed(value) => value.to_string(),
    //             Cow::Owned(value) => value.clone(),
    //         }),
    //     }
    // }
}

// impl<'a> PartialEq<JsonNumber<&'a str>> for JsonNumber<String> {
//     fn eq(&self, other: &JsonNumber<&'a str>) -> bool {
//         self.source == other.source
//     }
// }

#[test]
fn json_number_from_json() {
    assert_eq!(
        JsonNumber::from_json("1").unwrap(),
        JsonNumber { source: "1" }
    );

    let expected_number = JsonNumber::from_json(r#"true"#)
        .expect_err("shouldn't allow non-numbers")
        .kind;
    assert!(matches!(expected_number, ErrorKind::ExpectedNumber));
}

#[test]
fn json_number_conversions() {
    let one = JsonNumber::from_json("1").unwrap();
    assert_eq!(one.as_i64().unwrap(), 1);
    assert_eq!(one.as_u64().unwrap(), 1);
    assert!((one.as_f64().unwrap() - 1.0).abs() < f64::EPSILON);
}
