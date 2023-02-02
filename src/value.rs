#[cfg(feature = "alloc")]
use alloc::{string::String, vec::Vec};
use core::convert::Infallible;
use core::fmt::{self, Display};
use core::ops::{Deref, DerefMut, Index, IndexMut};

use crate::parser::{JsonKind, ParseConfig, ParseDelegate, Parser};
use crate::{Error, JsonNumber, JsonString};

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
    #[inline]
    pub const fn as_object(&self) -> Option<&Object<'a>> {
        if let Self::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns a mutable reference to the [`Object`] inside of this value, if
    /// this is a [`Value::Object`].
    #[must_use]
    #[inline]
    pub fn as_object_mut(&mut self) -> Option<&mut Object<'a>> {
        if let Self::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the contained value associated with `key`, if this is a
    /// [`Value::Object`]. Returns `None` if the value is not an object or if
    /// the key is not found.
    ///
    /// # Performance
    ///
    /// [`Object`] uses a `Vec` of [`Entry`] types to store its entries. If the
    /// operation being performed can be done with a single iteration over the
    /// value's contents instead of multiple random accesses, the iteration
    /// should be preferred. Additional options to make random access faster in
    /// environments that can support it [are being considered][issue] for
    /// future releases.
    ///
    /// [issue]: https://github.com/khonsulabs/justjson/issues/7
    #[must_use]
    #[inline]
    pub fn get(&self, key: &str) -> Option<&Value<'a>> {
        let object = self.as_object()?;
        object.get(key)
    }

    /// Returns a mutable reference to the contained value associated with
    /// `key`, if this is a [`Value::Object`]. Returns `None` if the value is
    /// not an object or if the key is not found.
    ///
    /// # Performance
    ///
    /// [`Object`] uses a `Vec` of [`Entry`] types to store its entries. If the
    /// operation being performed can be done with a single iteration over the
    /// value's contents instead of multiple random accesses, the iteration
    /// should be preferred. Additional options to make random access faster in
    /// environments that can support it [are being considered][issue] for
    /// future releases.
    ///
    /// [issue]: https://github.com/khonsulabs/justjson/issues/7
    #[must_use]
    #[inline]
    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value<'a>> {
        let object = self.as_object_mut()?;
        object.get_mut(key)
    }

    /// Returns the [`JsonString`] inside of this value, if this is a
    /// [`Value::String`].
    #[must_use]
    #[inline]
    pub const fn as_string(&self) -> Option<&JsonString<'a>> {
        if let Self::String(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the [`JsonNumber`] inside of this value, if this is a
    /// [`Value::Number`].
    #[must_use]
    #[inline]
    pub const fn as_number(&self) -> Option<&JsonNumber<'a>> {
        if let Self::Number(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    /// Returns the `bool` inside of this value, if this is a
    /// [`Value::Boolean`].
    #[must_use]
    #[inline]
    pub const fn as_bool(&self) -> Option<bool> {
        if let Self::Boolean(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the slice of values inside of this value, if this is a
    /// [`Value::Array`].
    #[must_use]
    #[inline]
    pub fn as_array(&self) -> Option<&[Self]> {
        if let Self::Array(value) = self {
            Some(value)
        } else {
            None
        }
    }

    /// Returns a mutable reference to the Vec of values inside of this value,
    /// if this is a [`Value::Array`].
    #[must_use]
    #[inline]
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Self>> {
        if let Self::Array(value) = self {
            Some(value)
        } else {
            None
        }
    }

    /// Returns the contained value at `index`, if this is a [`Value::Array`].
    /// Returns `None` if the value is not an array or if `index` is beyond the
    /// bounds of the array.
    #[must_use]
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<&Value<'a>> {
        let sequence = self.as_array()?;
        sequence.get(index)
    }

    /// Returns a mutable reference to the contained value at `index`, if this
    /// is a [`Value::Array`]. Returns `None` if the value is not an array or if
    /// `index` is beyond the bounds of the array.
    #[must_use]
    #[inline]
    pub fn get_index_mut(&mut self, index: usize) -> Option<&mut Value<'a>> {
        let sequence = self.as_array_mut()?;
        sequence.get_mut(index)
    }

    /// Returns true if this value is `null`/[`Value::Null`].
    #[must_use]
    #[inline]
    pub const fn is_null(&self) -> bool {
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
            Value::String(string) => state.write_json(string),
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
                state.write_json(&entry.key)?;
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.pretty_write_json_to(f)
        } else {
            self.write_json_to(f)
        }
    }
}

pub(crate) struct ValueParser;

impl<'a> ParseDelegate<'a> for ValueParser {
    type Array = Vec<Value<'a>>;
    type Error = Infallible;
    type Key = JsonString<'a>;
    type Object = Object<'a>;
    type Value = Value<'a>;

    #[inline]
    fn null(&mut self) -> Result<Self::Value, Self::Error> {
        Ok(Value::Null)
    }

    #[inline]
    fn boolean(&mut self, value: bool) -> Result<Self::Value, Self::Error> {
        Ok(Value::Boolean(value))
    }

    #[inline]
    fn number(&mut self, value: JsonNumber<'a>) -> Result<Self::Value, Self::Error> {
        Ok(Value::Number(value))
    }

    #[inline]
    fn string(&mut self, value: JsonString<'a>) -> Result<Self::Value, Self::Error> {
        Ok(Value::String(value))
    }

    #[inline]
    fn begin_object(&mut self) -> Result<Self::Object, Self::Error> {
        Ok(Object::default())
    }

    #[inline]
    fn object_key(
        &mut self,
        _object: &mut Self::Object,
        key: JsonString<'a>,
    ) -> Result<Self::Key, Self::Error> {
        Ok(key)
    }

    #[inline]
    fn object_value(
        &mut self,
        object: &mut Self::Object,
        key: Self::Key,
        value: Self::Value,
    ) -> Result<(), Self::Error> {
        object.push(Entry { key, value });
        Ok(())
    }

    #[inline]
    fn object_is_empty(&self, object: &Self::Object) -> bool {
        object.is_empty()
    }

    #[inline]
    fn end_object(&mut self, object: Self::Object) -> Result<Self::Value, Self::Error> {
        Ok(Value::Object(object))
    }

    #[inline]
    fn begin_array(&mut self) -> Result<Self::Array, Self::Error> {
        Ok(Vec::new())
    }

    #[inline]
    fn array_value(
        &mut self,
        array: &mut Self::Array,
        value: Self::Value,
    ) -> Result<(), Self::Error> {
        array.push(value);
        Ok(())
    }

    #[inline]
    fn array_is_empty(&self, array: &Self::Array) -> bool {
        array.is_empty()
    }

    #[inline]
    fn end_array(&mut self, array: Self::Array) -> Result<Self::Value, Self::Error> {
        Ok(Value::Array(array))
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

    fn write_json(&mut self, str: &JsonString<'_>) -> fmt::Result {
        if PRETTY && self.is_at_line_start {
            self.is_at_line_start = false;

            for _ in 0..self.level {
                self.writer.write_str(self.indent_per_level)?;
            }
        }

        write!(self.writer, "\"{}\"", str.as_json())
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

impl<'a> Index<usize> for Value<'a> {
    type Output = Value<'a>;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        let sequence = self.as_array().expect("value is not an array");
        &sequence[index]
    }
}

impl<'a> IndexMut<usize> for Value<'a> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let sequence = self.as_array_mut().expect("value is not an array");
        &mut sequence[index]
    }
}

impl<'b, 'a> Index<&'b str> for Value<'a> {
    type Output = Value<'a>;

    #[inline]
    fn index(&self, index: &'b str) -> &Self::Output {
        self.get(index).expect("key not found")
    }
}

impl<'b, 'a> IndexMut<&'b str> for Value<'a> {
    #[inline]
    fn index_mut(&mut self, index: &'b str) -> &mut Self::Output {
        self.get_mut(index).expect("key not found")
    }
}

/// A JSON Object (list of key-value pairs).
///
/// # Performance
///
/// [`Object`] uses a `Vec` of [`Entry`] types to store its entries. If the
/// operation being performed can be done with a single iteration over the
/// value's contents instead of multiple random accesses, the iteration
/// should be preferred. Additional options to make random access faster in
/// environments that can support it [are being considered][issue] for
/// future releases.
///
/// [issue]: https://github.com/khonsulabs/justjson/issues/7
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

    /// Returns the value associated with `key`, if found.
    #[must_use]
    #[inline]
    pub fn get(&self, key: &str) -> Option<&Value<'a>> {
        self.iter()
            .find_map(|entry| (entry.key == key).then_some(&entry.value))
    }

    /// Returns a mutable reference to the value associated with `key`, if
    /// found.
    #[must_use]
    #[inline]
    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value<'a>> {
        self.get_entry_mut(key).map(|entry| &mut entry.value)
    }

    /// Returns a mutable reference to the entry associated with `key`, if
    /// found.
    #[must_use]
    #[inline]
    pub fn get_entry_mut(&mut self, key: &str) -> Option<&mut Entry<'a>> {
        self.iter_mut().find(|entry| entry.key == key)
    }
}

impl<'a> Deref for Object<'a> {
    type Target = Vec<Entry<'a>>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Object<'a> {
    #[inline]
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

/// An key-value pair in an [`Object`].
#[derive(Debug, Eq, PartialEq)]
pub struct Entry<'a> {
    /// The key of this entry.
    pub key: JsonString<'a>,
    /// The value associated with the key.
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
    value["b"] = Value::Boolean(false);
    let root = value.as_object_mut().unwrap();
    root[0].key = JsonString::from("newa");
    let Value::Array(d_array) = &mut root[3].value else { unreachable!() };
    d_array.push(Value::Null);

    // Replace the newly inserted null (uses IndexMut on the array).
    value["d"][0] = Value::Boolean(false);

    let generated = value.to_json();
    assert_eq!(
        generated,
        r#"{"newa":1,"b":false,"c":"hello","d":[false],"e":{}}"#
    );
}

#[test]
fn index() {
    let value = Value::from_json_bytes(br#"{"b":true,"a":[false]}"#).unwrap();
    assert_eq!(value["b"], Value::Boolean(true));
    assert_eq!(value["a"][0], Value::Boolean(false));
}
