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
pub enum Value<'a> {
    /// A JSON number.
    Number(JsonNumber<'a>),
    /// A JSON string.
    String(JsonString<'a>),
    /// A boolean value.
    Boolean(bool),
    /// A JSON object (key/value pairs).
    Object(Object<'a>),
    /// A JSON array (list of values).
    Array(Vec<Value<'a>>),
    /// A null value.
    Null,
}

impl<'a> Value<'a> {
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
        Parser::parse_json_with_config(json, config, ValueParser)
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
        Parser::parse_json_bytes_with_config(json, config, ValueParser)
    }

    /// Returns the [`Object`] inside of this value, if this is a
    /// [`Value::Object`].
    #[must_use]
    pub fn as_object(&self) -> Option<&Object<'a>> {
        if let Self::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the [`JsonString`] inside of this value, if this is a
    /// [`Value::String`].
    #[must_use]
    pub fn as_string(&self) -> Option<&JsonString<'a>> {
        if let Self::String(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the [`JsonNumber`] inside of this value, if this is a
    /// [`Value::Number`].
    #[must_use]
    pub fn as_number(&self) -> Option<&JsonNumber<'a>> {
        if let Self::Number(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the `bool` inside of this value, if this is a
    /// [`Value::Boolean`].
    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Boolean(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the slice of values inside of this value, if this is a
    /// [`Value::Array`].
    #[must_use]
    pub fn as_array(&self) -> Option<&[Self]> {
        if let Self::Array(value) = self {
            Some(value)
        } else {
            None
        }
    }

    /// Returns true if this value is `null`/[`Value::Null`].
    #[must_use]
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
            Value::String(string) => {
                state.write("\"")?;
                state.write(string.contents())?;
                state.write("\"")
            }
            Value::Number(number) => state.write(number.source()),
            Value::Boolean(bool) => state.write(if *bool { "true" } else { "false" }),
            Value::Null => state.write("null"),
            Value::Object(obj) => Self::write_json_object(obj, state),
            Value::Array(array) => Self::write_json_array(array, state),
        }
    }

    fn write_json_object<W: fmt::Write, const PRETTY: bool>(
        obj: &Object<'_>,
        state: &mut WriteState<'_, W, PRETTY>,
    ) -> fmt::Result {
        state.begin_object()?;

        if !obj.0.is_empty() {
            state.new_line()?;
            for (index, entry) in obj.0.iter().enumerate() {
                state.write("\"")?;
                state.write(entry.key.contents())?;
                state.write_object_key_end()?;
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
    #[must_use]
    pub fn to_json_pretty(&self) -> String {
        let mut out = String::new();
        self.pretty_write_json_to(&mut out).expect("out of memory");
        out
    }

    /// Converts this value to its JSON representation, with extra whitespace to
    /// make it easier for a human to read.
    #[must_use]
    pub fn to_json_pretty_custom(&self, indentation: &str, line_ending: &str) -> String {
        let mut out = String::new();
        self.pretty_write_json_to_custom(indentation, line_ending, &mut out)
            .expect("out of memory");
        out
    }

    /// Converts this value to its JSON representation, with no extraneous
    /// whitespace.
    #[must_use]
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
    assert!(Value::Boolean(true).as_bool().unwrap());
    assert_eq!(
        Value::String(JsonString::from_json("\"\"").unwrap())
            .as_string()
            .unwrap(),
        ""
    );
    assert_eq!(
        Value::Number(JsonNumber::from_json("1").unwrap())
            .as_number()
            .unwrap()
            .as_u64()
            .unwrap(),
        1
    );
    assert_eq!(
        Value::Object(Object::new()).as_object().unwrap(),
        &Object::new()
    );
    assert_eq!(Value::Array(Vec::new()).as_array().unwrap(), &[]);

    assert!(Value::Null.is_null());
    assert!(!Value::Boolean(true).is_null());
    assert_eq!(Value::Null.as_bool(), None);
    assert_eq!(Value::Null.as_number(), None);
    assert_eq!(Value::Null.as_string(), None);
    assert_eq!(Value::Null.as_object(), None);
    assert_eq!(Value::Null.as_array(), None);
}

impl<'a> Display for Value<'a> {
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
    type Array = Vec<Value<'a>>;
    type Key = JsonString<'a>;
    type Object = Object<'a>;
    type Value = Value<'a>;

    #[inline]
    fn null(&mut self) -> Self::Value {
        Value::Null
    }

    #[inline]
    fn boolean(&mut self, value: bool) -> Self::Value {
        Value::Boolean(value)
    }

    #[inline]
    fn number(&mut self, value: JsonNumber<'a>) -> Self::Value {
        Value::Number(value)
    }

    #[inline]
    fn string(&mut self, value: JsonString<'a>) -> Self::Value {
        Value::String(value)
    }

    #[inline]
    fn begin_object(&mut self) -> Self::Object {
        Object::default()
    }

    #[inline]
    fn object_key(&mut self, _object: &mut Self::Object, key: JsonString<'a>) -> Self::Key {
        key
    }

    #[inline]
    fn object_value(&mut self, object: &mut Self::Object, key: Self::Key, value: Self::Value) {
        object.push(Entry { key, value });
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

    fn write_object_key_end(&mut self) -> fmt::Result {
        if PRETTY {
            self.write("\": ")?;
        } else {
            self.write("\":")?;
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

/// A JSON Object (list of key-value pairs).
#[derive(Debug, Eq, PartialEq)]
pub struct Object<'a>(Vec<Entry<'a>>);

impl<'a> Default for Object<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Object<'a> {
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
}

impl<'a> Deref for Object<'a> {
    type Target = Vec<Entry<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Object<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> FromIterator<(JsonString<'a>, Value<'a>)> for Object<'a> {
    fn from_iter<T: IntoIterator<Item = (JsonString<'a>, Value<'a>)>>(iter: T) -> Self {
        iter.into_iter()
            .map(|(key, value)| Entry { key, value })
            .collect()
    }
}

impl<'a> FromIterator<Entry<'a>> for Object<'a> {
    fn from_iter<T: IntoIterator<Item = Entry<'a>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Entry<'a> {
    pub key: JsonString<'a>,
    pub value: Value<'a>,
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
fn cow() {
    let mut value =
        Value::from_json_bytes(br#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#).unwrap();
    let Value::Object(root) = &mut value else { unreachable!() };
    root[0].key = JsonString::from("newa");
    let Value::Array(d_array) = &mut root[3].value else { unreachable!() };
    d_array.push(Value::Null);
    let generated = value.to_json();
    assert_eq!(
        generated,
        r#"{"newa":1,"b":true,"c":"hello","d":[null],"e":{}}"#
    );
}
