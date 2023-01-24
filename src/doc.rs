#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use core::iter::Cloned;
use core::marker::PhantomData;
use core::slice;

use crate::parser::{JsonKind, ParseConfig, ParseDelegate, Parser};
#[cfg(feature = "alloc")]
use crate::{value::Entry, Object, Value};
use crate::{Error, ErrorKind, JsonNumber, JsonString};

/// A parsed JSON payload.
///
/// This type is a read-only view into the JSON payload.
///
/// This structure is faster to parse than [`Value`], but it is more work to
/// interact with because the entire payload is stored in a single list
/// internally.
///
/// The list of [`Node`]s is sequentially ordered by the order in which they
/// appear in the document. This means that the first [`Node`] tells us what the
/// root type of the document is.
///
/// The nodes [`Node::Null`], [`Node::Boolean`], [`Node::String`], and
/// [`Node::Number`] all directly represent a [`Value`] with the same name.
///
/// [`Node::Object`] contains a length field, which lets us know how many
/// key-value pairs will appear after the object's node. Object keys are
/// guaranteed to be [`Node::String`]s, but object values can be any [`Node`]
/// variant. This means care must be taken to handle nested structures.
///
/// [`Node::Array`] also contains a length field, which lets us know how many
/// values follow the array's node. Because array values can be any type, care
/// must be taken to handle nested structures correctly.
///
/// [`DocumentIter`] has multiple methods to help efficiently deal with nested
/// data types:
///
/// - [`skip_next_value()`](DocumentIter::skip_next_value): Skips over the next
///   value, taking care to handle nested structures properly.
/// - [`next_value()`](DocumentIter::next_value): Returns a [`Value`] from the
///   iterator.
#[derive(Debug, Eq, PartialEq)]
pub struct GenericDocument<'a, Backing> {
    nodes: Nodes<'a, Backing>,
}

impl<'a, Backing> GenericDocument<'a, Backing>
where
    Backing: NodeCollection<'a>,
{
    /// Parses a JSON payload from `source`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn from_json_bytes(source: &'a [u8]) -> Result<Self, Error>
    where
        Backing: Default,
    {
        let mut nodes = Nodes::default();
        Parser::parse_json_bytes(source, &mut nodes).map_err(Error::into_infallable)?;
        Ok(Self { nodes })
    }

    /// Parses a JSON payload from `source`, with the settings from `config`.
    ///
    /// This function verifies that `json` is valid UTF-8 while parsing the
    /// JSON.
    pub fn from_json_bytes_with_config(source: &'a [u8], config: ParseConfig) -> Result<Self, Error>
    where
        Backing: Default,
    {
        let mut nodes = Nodes::default();
        Parser::parse_json_bytes_with_config(source, config, &mut nodes)
            .map_err(Error::into_infallable)?;
        Ok(Self { nodes })
    }

    /// Parses a JSON payload from `source`.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn from_json(source: &'a str) -> Result<Self, Error>
    where
        Backing: Default,
    {
        let mut nodes = Nodes::default();
        Parser::parse_json(source, &mut nodes).map_err(Error::into_infallable)?;
        Ok(Self { nodes })
    }

    /// Parses a JSON payload from `source`, with the settings from `config`.
    ///
    /// Because the `str` type guarantees that `json` is valid UTF-8, no
    /// additional unicode checks are performed on unescaped unicode sequences.
    pub fn from_json_with_config(source: &'a str, config: ParseConfig) -> Result<Self, Error>
    where
        Backing: Default,
    {
        let mut nodes = Nodes::default();
        Parser::parse_json_with_config(source, config, &mut nodes)
            .map_err(Error::into_infallable)?;
        Ok(Self { nodes })
    }

    /// Returns an iterator over the nodes in this document.
    #[must_use]
    pub fn iter(&self) -> DocumentIter<'_, 'a> {
        self.into_iter()
    }
}

#[cfg(feature = "alloc")]
impl<'a, Backing> From<GenericDocument<'a, Backing>> for Value<'a>
where
    Backing: NodeCollection<'a>,
{
    fn from(doc: GenericDocument<'a, Backing>) -> Self {
        let mut nodes = doc.nodes.collection.into_iter();
        let root = nodes.next().expect("empty document is invalid");
        hydrate_value_from_node(root, &mut nodes)
    }
}

#[cfg(feature = "alloc")]
fn hydrate_value_from_node<'a, I>(node: Node<'a>, remaining_nodes: &mut I) -> Value<'a>
where
    I: Iterator<Item = Node<'a>>,
{
    match node {
        Node::Null => Value::Null,
        Node::Boolean(value) => Value::Boolean(value),
        Node::String(value) => Value::String(value),
        Node::Number(number) => Value::Number(number),
        Node::Object { length: len } => {
            let mut obj = Object::with_capacity(len);
            for _ in 0..len {
                let node = remaining_nodes.next().expect("obbject missing value");
                let Node::String(key) = node else { unreachable!("object key must be string") };
                let node = remaining_nodes.next().expect("object missing value");
                obj.push(Entry {
                    key,
                    value: hydrate_value_from_node(node, remaining_nodes),
                });
            }
            Value::Object(obj)
        }
        Node::Array { length: len } => {
            let mut values = Vec::with_capacity(len);
            for _ in 0..len {
                let node = remaining_nodes.next().expect("array missing value");
                values.push(hydrate_value_from_node(node, remaining_nodes));
            }
            Value::Array(values)
        }
    }
}

impl<'doc, 'a, Backing> IntoIterator for &'doc GenericDocument<'a, Backing>
where
    Backing: NodeCollection<'a>,
{
    type IntoIter = DocumentIter<'doc, 'a>;
    type Item = Node<'a>;

    fn into_iter(self) -> Self::IntoIter {
        DocumentIter {
            nodes: self.nodes.collection.as_ref().iter().cloned(),
        }
    }
}

/// A single value in a [`Document`].
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node<'a> {
    /// A null value.
    Null,
    /// A boolean value.
    Boolean(bool),
    /// A string value.
    String(JsonString<'a>),
    /// A numerical value.
    Number(JsonNumber<'a>),
    /// An object value with `len` key-value pairs following it.
    Object {
        /// The number of key-value pairs that this object contains.
        length: usize,
    },
    /// An array with `len` values following it.
    Array {
        /// The number of values that this array contains.
        length: usize,
    },
}

#[derive(Default, Debug, Eq, PartialEq)]
struct Nodes<'a, Backing> {
    collection: Backing,
    _phantom: PhantomData<&'a ()>,
}

impl<'a, Backing> Nodes<'a, Backing>
where
    Backing: NodeCollection<'a>,
{
    fn push_node(&mut self, node: Node<'a>) -> Result<usize, ErrorKind> {
        let index = self.collection.as_ref().len();
        self.collection.push(node)?;
        Ok(index)
    }

    pub fn push_null(&mut self) -> Result<usize, ErrorKind> {
        self.push_node(Node::Null)
    }

    pub fn push_bool(&mut self, boolean: bool) -> Result<usize, ErrorKind> {
        self.push_node(Node::Boolean(boolean))
    }

    pub fn push_string(&mut self, string: JsonString<'a>) -> Result<usize, ErrorKind> {
        self.push_node(Node::String(string))
    }

    pub fn push_number(&mut self, number: JsonNumber<'a>) -> Result<usize, ErrorKind> {
        self.push_node(Node::Number(number))
    }

    pub fn push_object(&mut self) -> Result<usize, ErrorKind> {
        self.push_node(Node::Object { length: 0 })
    }

    pub fn push_array(&mut self) -> Result<usize, ErrorKind> {
        self.push_node(Node::Array { length: 0 })
    }

    pub fn extend_object(&mut self, index: usize) {
        let Node::Object { length: len } = &mut self.collection.as_mut()[index] else { unreachable!("extended wrong type") };
        *len += 1;
    }

    pub fn extend_array(&mut self, index: usize) {
        let Node::Array { length: len } = &mut self.collection.as_mut()[index] else { unreachable!("extended wrong type") };
        *len += 1;
    }
}

impl<'a, 'b, Backing> ParseDelegate<'a> for &'b mut Nodes<'a, Backing>
where
    Backing: NodeCollection<'a>,
{
    type Array = usize;
    type Error = ErrorKind;
    type Key = ();
    type Object = usize;
    type Value = usize;

    #[inline]
    fn null(&mut self) -> Result<Self::Value, Self::Error> {
        self.push_null()
    }

    #[inline]
    fn boolean(&mut self, value: bool) -> Result<Self::Value, Self::Error> {
        self.push_bool(value)
    }

    #[inline]
    fn number(&mut self, value: JsonNumber<'a>) -> Result<Self::Value, Self::Error> {
        self.push_number(value)
    }

    #[inline]
    fn string(&mut self, value: JsonString<'a>) -> Result<Self::Value, Self::Error> {
        self.push_string(value)
    }

    #[inline]
    fn begin_object(&mut self) -> Result<Self::Object, Self::Error> {
        self.push_object()
    }

    #[inline]
    fn object_key(
        &mut self,
        object: &mut Self::Object,
        key: JsonString<'a>,
    ) -> Result<Self::Key, Self::Error> {
        self.extend_object(*object);
        self.push_string(key)?;
        Ok(())
    }

    #[inline]
    fn object_value(
        &mut self,
        _object: &mut Self::Object,
        _key: Self::Key,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    #[inline]
    fn object_is_empty(&self, object: &Self::Object) -> bool {
        let Node::Object { length: len } = &self.collection.as_ref()[*object] else { unreachable!("invalid object") };
        *len == 0
    }

    #[inline]
    fn end_object(&mut self, object: Self::Object) -> Result<Self::Value, Self::Error> {
        Ok(object)
    }

    #[inline]
    fn begin_array(&mut self) -> Result<Self::Array, Self::Error> {
        self.push_array()
    }

    #[inline]
    fn array_value(
        &mut self,
        array: &mut Self::Array,
        _value: Self::Value,
    ) -> Result<(), Self::Error> {
        self.extend_array(*array);
        Ok(())
    }

    #[inline]
    fn array_is_empty(&self, array: &Self::Array) -> bool {
        let Node::Array { length: len } = &self.collection.as_ref()[*array] else { unreachable!("invalid array") };
        *len == 0
    }

    #[inline]
    fn end_array(&mut self, array: Self::Array) -> Result<Self::Value, Self::Error> {
        Ok(array)
    }

    #[inline]
    fn kind_of(&self, value: &Self::Value) -> JsonKind {
        match &self.collection.as_ref()[*value] {
            Node::Null => JsonKind::Null,
            Node::Boolean(_) => JsonKind::Boolean,
            Node::String(_) => JsonKind::String,
            Node::Number(_) => JsonKind::Number,
            Node::Object { .. } => JsonKind::Object,
            Node::Array { .. } => JsonKind::Array,
        }
    }
}

/// An iterator over the [`Node`]s in a [`Document`].
#[derive(Debug, Clone)]
pub struct DocumentIter<'doc, 'a> {
    nodes: Cloned<slice::Iter<'doc, Node<'a>>>,
}

impl<'doc, 'a> DocumentIter<'doc, 'a> {
    /// Reads a [`Value`] from the iterator, if any nodes remain.
    ///
    /// This function automatically reads nested objects and arrays.
    ///
    /// ```rust
    /// use justjson::doc::{Document, Node};
    /// use justjson::Value;
    ///
    /// let doc = Document::from_json(
    ///     r#"{
    ///         "a": [1, 2, 3, 4]
    ///     }"#,
    /// )
    /// .unwrap();
    ///
    /// let mut nodes = doc.iter();
    /// assert_eq!(nodes.next(), Some(Node::Object { length: 1 }));
    /// let Node::String(key) = nodes.next().unwrap() else { unreachable!() };
    /// assert_eq!(key, "a");
    /// let Value::Array(array) = nodes.next_value().unwrap() else { unreachable!() };
    /// assert_eq!(array.len(), 4);
    /// assert!(nodes.next().is_none());
    /// ```
    #[cfg(feature = "alloc")]
    pub fn next_value(&mut self) -> Option<Value<'a>> {
        let node = self.nodes.next()?;

        Some(hydrate_value_from_node(node, &mut self.nodes))
    }

    /// Skips a [`Value`], including any nested values.
    pub fn skip_next_value(&mut self) {
        if let Some(node) = self.nodes.next() {
            match node {
                Node::Null | Node::Boolean(_) | Node::String(_) | Node::Number(_) => {}
                Node::Object { length: len } => {
                    for _ in 0..len {
                        // Skip the key
                        self.skip_next_value();
                        // Skip the value
                        self.skip_next_value();
                    }
                }
                Node::Array { length: len } => {
                    for _ in 0..len {
                        // Skip the value
                        self.skip_next_value();
                    }
                }
            }
        }
    }
}

impl<'doc, 'a> Iterator for DocumentIter<'doc, 'a> {
    type Item = Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.nodes.next()
    }
}

#[test]
#[cfg(feature = "alloc")]
fn document_iteration() {
    let source = r#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#;
    let doc = Document::from_json(source).unwrap();
    assert_eq!(
        doc.iter().collect::<Vec<_>>(),
        alloc::vec![
            Node::Object { length: 5 },
            Node::String(JsonString::from_json("\"a\"").unwrap()),
            Node::Number(JsonNumber::from_json("1").unwrap()),
            Node::String(JsonString::from_json("\"b\"").unwrap()),
            Node::Boolean(true),
            Node::String(JsonString::from_json("\"c\"").unwrap()),
            Node::String(JsonString::from_json("\"hello\"").unwrap()),
            Node::String(JsonString::from_json("\"d\"").unwrap()),
            Node::Array { length: 0 },
            Node::String(JsonString::from_json("\"e\"").unwrap()),
            Node::Object { length: 0 },
        ]
    );
    let value = doc.iter().next_value().unwrap();
    assert_eq!(value, Value::from_json(source).unwrap());

    // Test skipping
    let mut iter = doc.iter();
    iter.skip_next_value();
    assert!(iter.next().is_none());
}

/// A collection for use with [`GenericDocument`].
pub trait NodeCollection<'a>:
    AsRef<[Node<'a>]> + AsMut<[Node<'a>]> + IntoIterator<Item = Node<'a>>
{
    /// Push `node` to the end of the collection.
    fn push(&mut self, node: Node<'a>) -> Result<(), ErrorKind>;
}

#[cfg(feature = "alloc")]
impl<'a> NodeCollection<'a> for Vec<Node<'a>> {
    #[inline]
    fn push(&mut self, node: Node<'a>) -> Result<(), ErrorKind> {
        self.push(node);
        Ok(())
    }
}

#[cfg(feature = "heapless")]
impl<'a, const N: usize> NodeCollection<'a> for heapless::Vec<Node<'a>, N> {
    #[inline]
    fn push(&mut self, node: Node<'a>) -> Result<(), ErrorKind> {
        self.push(node).map_err(|_| ErrorKind::PaylodTooLarge)
    }
}

/// A convenience alias for a [`GenericDocument`] that uses a `Vec` from
/// `std`/`alloc`.
#[cfg(feature = "alloc")]
pub type Document<'a> = GenericDocument<'a, Vec<Node<'a>>>;

/// A convenience alias for a [`GenericDocument`] that uses a `heapless::Vec`.
#[cfg(feature = "heapless")]
pub type HeaplessDocument<'a, const N: usize> = GenericDocument<'a, heapless::Vec<Node<'a>, N>>;
