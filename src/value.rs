use std::{
    fmt::{self, Display},
    ops::{Deref, DerefMut},
};

use crate::{
    parser::{JsonKind, ParseConfig, ParseDelegate, Parser},
    Error, JsonNumber, JsonString,
};

/// A JSON value.
///
/// The `Backing` generic is the storage mechanism used by [`JsonNumber`] and
/// [`JsonString`]. This is generally `&str` or `Cow<str>`.
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
        Parser::parse_str_with_config(json, config, ValueParser)
    }

    /// Parses a JSON value from `json`, returning a `Value<&str>` that borrows
    /// data from `json`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn from_json_bytes(json: &'a [u8]) -> Result<Self, Error> {
        Self::from_json_bytes_with_config(json, ParseConfig::default())
    }

    /// Parses a JSON value from `json` using the settings from`config`,
    /// returning a `Value<&str>` that borrows data from `json`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn from_json_bytes_with_config(json: &'a [u8], config: ParseConfig) -> Result<Self, Error> {
        Parser::parse_with_config(json, config, ValueParser)
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

impl<Backing> Value<Backing>
where
    Backing: AsRef<str>,
{
    /// Returns the [`Object`] inside of this value, if this is a
    /// [`Value::Object`].
    pub fn as_object(&self) -> Option<&Object<Backing>> {
        if let Self::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the [`JsonString`] inside of this value, if this is a
    /// [`Value::String`].
    pub fn as_string(&self) -> Option<&JsonString<Backing>> {
        if let Self::String(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the [`JsonNumber`] inside of this value, if this is a
    /// [`Value::Number`].
    pub fn as_number(&self) -> Option<&JsonNumber<Backing>> {
        if let Self::Number(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the `bool` inside of this value, if this is a
    /// [`Value::Boolean`].
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Boolean(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the slice of values inside of this value, if this is a
    /// [`Value::Array`].
    pub fn as_array(&self) -> Option<&[Self]> {
        if let Self::Array(value) = self {
            Some(value)
        } else {
            None
        }
    }

    /// Returns true if this value is `null`/[`Value::Null`].
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

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
            for (index, entry) in obj.0.iter().enumerate() {
                state.write(entry.key.source.as_ref())?;
                state.write_colon()?;
                entry.value.write_json_value(state)?;
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

#[test]
fn value_ases() {
    assert!(Value::<&str>::Boolean(true).as_bool().unwrap());
    assert_eq!(
        Value::<&str>::String(JsonString::from_json("\"\"").unwrap())
            .as_string()
            .unwrap(),
        ""
    );
    assert_eq!(
        Value::<&str>::Number(JsonNumber::from_json("1").unwrap())
            .as_number()
            .unwrap()
            .as_u64()
            .unwrap(),
        1
    );
    assert_eq!(
        Value::<&str>::Object(Object::new()).as_object().unwrap(),
        &Object::new()
    );
    assert_eq!(Value::<&str>::Array(Vec::new()).as_array().unwrap(), &[]);

    assert!(Value::<&str>::Null.is_null());
    assert!(!Value::<&str>::Boolean(true).is_null());
    assert_eq!(Value::<&str>::Null.as_bool(), None);
    assert_eq!(Value::<&str>::Null.as_number(), None);
    assert_eq!(Value::<&str>::Null.as_string(), None);
    assert_eq!(Value::<&str>::Null.as_object(), None);
    assert_eq!(Value::<&str>::Null.as_array(), None);
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

struct ValueParser;

impl<'a> ParseDelegate<'a> for ValueParser {
    type Value = Value<&'a str>;
    type Object = Object<&'a str>;
    type Array = Vec<Value<&'a str>>;
    type Key = JsonString<&'a str>;

    #[inline]
    fn null(&mut self) -> Self::Value {
        Value::Null
    }

    #[inline]
    fn boolean(&mut self, value: bool) -> Self::Value {
        Value::Boolean(value)
    }

    #[inline]
    fn number(&mut self, value: JsonNumber<&'a str>) -> Self::Value {
        Value::Number(value)
    }

    #[inline]
    fn string(&mut self, value: JsonString<&'a str>) -> Self::Value {
        Value::String(value)
    }

    #[inline]
    fn begin_object(&mut self) -> Self::Object {
        Object::default()
    }

    #[inline]
    fn object_key(&mut self, _object: &mut Self::Object, key: JsonString<&'a str>) -> Self::Key {
        key
    }

    #[inline]
    fn object_value(&mut self, object: &mut Self::Object, key: Self::Key, value: Self::Value) {
        object.push(key, value);
    }

    #[inline]
    fn object_is_empty(&self, object: &Self::Object) -> bool {
        object.is_empty()
    }

    #[inline]
    fn end_object(&mut self, object: Self::Object) -> Self::Value {
        Value::Object(object)
    }

    #[inline]
    fn begin_array(&mut self) -> Self::Array {
        Vec::new()
    }

    #[inline]
    fn array_value(&mut self, array: &mut Self::Array, value: Self::Value) {
        array.push(value);
    }

    #[inline]
    fn array_is_empty(&self, array: &Self::Array) -> bool {
        array.is_empty()
    }

    #[inline]
    fn end_array(&mut self, array: Self::Array) -> Self::Value {
        Value::Array(array)
    }

    #[inline]
    fn kind_of(&self, value: &Self::Value) -> JsonKind {
        match value {
            Value::Number(_) => JsonKind::Number,
            Value::String(_) => JsonKind::String,
            Value::Boolean(_) => JsonKind::Boolean,
            Value::Object(_) => JsonKind::Object,
            Value::Array(_) => JsonKind::Array,
            Value::Null => JsonKind::Null,
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

// impl<'a> PartialEq<Value<&'a str>> for Value<String> {
//     fn eq(&self, other: &Value<&'a str>) -> bool {
//         match (self, other) {
//             (Self::Number(l0), Value::Number(r0)) => l0 == r0,
//             (Self::String(l0), Value::String(r0)) => l0 == r0,
//             (Self::Boolean(l0), Value::Boolean(r0)) => l0 == r0,
//             (Self::Object(l0), Value::Object(r0)) => l0 == r0,
//             (Self::Array(l0), Value::Array(r0)) => l0 == r0,
//             (Self::Null, Value::Null) => true,
//             _ => false,
//         }
//     }
// }

/// A JSON Object (list of key-value pairs).
#[derive(Debug, Eq, PartialEq)]
pub struct Object<Backing>(Vec<Entry<Backing>>);

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

    /// Returns an empty object that can store up to `capacity` elements without
    /// reallocating.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    /// Adds a new key-value pair to this object.
    pub fn push(&mut self, key: JsonString<Backing>, value: Value<Backing>) {
        self.0.push(Entry { key, value });
    }
}

impl<Backing> Deref for Object<Backing> {
    type Target = [Entry<Backing>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Backing> DerefMut for Object<Backing> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
        iter.into_iter()
            .map(|(key, value)| Entry { key, value })
            .collect()
    }
}

impl<Backing> FromIterator<Entry<Backing>> for Object<Backing> {
    fn from_iter<T: IntoIterator<Item = Entry<Backing>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

// impl<'a> PartialEq<Object<&'a str>> for Object<String> {
//     fn eq(&self, other: &Object<&'a str>) -> bool {
//         self.0.len() == other.0.len()
//             && self
//                 .0
//                 .iter()
//                 .zip(other.0.iter())
//                 .all(|(a, b)| a.key == b.key && a.value == b.value)
//     }
// }

#[derive(Debug, Eq, PartialEq)]
pub struct Entry<Backing> {
    pub key: JsonString<Backing>,
    pub value: Value<Backing>,
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
