# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## Changed

- This crate's MSRV has been changed to 1.65. This is not a breaking change,
  however, as the crate never supported its advertised MSRV, and this new MSRV
  reflects what the true MSRV has always been. An additional CI pass was added
  to ensure the MSRV is always tested against. Thanks to @hr8 in #8 for
  reporting this.

## Added

- `AnyStr::as_str()` has been added, which acts the same as
  `AsRef<str>`/`Borrow<str>`/`Deref<Target = str>`.
- `JsonString::as_str()` has been added, which will return an `&str` to the
  contained string, if it doesn't contain escape sequences. If the string has
  escape sequences, None will be returned.
- `Value::as_str()` has been added, which will return an `&str` to the contained
  string if the the `Value` is a `JsonString` that has no escape sequences. If
  the value is not a string or it has escape sequences, None will be returned.

  Thanks to @hr8 in #9 for requesting this.
- `JsonString` now implements `Display` with the decoded representation. This
  also means that `ToString` is now implemented such that it produces a decoded
  string.

## v0.2.2

## Added

- `JsonString` now implements `Ord`, `PartialOrd`, and `PartialOrd<str>`, and
  `Hash`.
- `Entry`, the type which `Object` is a collection of, is now exported. This was
  an oversight. Thanks to @PoiScript for reporting this in #6.
- `Value::get(key)`, `Value::get_mut(key)`, and `Index<&str>`/`IndexMut<&str>`
  have been added to allow easier access to the contents of a `Value` when it is
  an object. Similar to most built-in collection types, the `Index` trait
  implementations will panic if the key is not found, but the `get` variants
  return an Option instead of panicking.
- `Value::get_index(usize)`, `Value::get_index_mut(usize)`, and
  `Index<usize>`/`IndexMut<usize>` have been added to allow easier access to the
  contents of a `Value` when it is an array. Similar to most built-in collection
  types, the `Index` trait implementations will panic if the index is out of
  bounds, but the `get_index` variants return an Option instead of panicking.
- `From<&str>`, `From<String>`, `From<JsonString<'_>>`, `From<Object<'_>>`, and
  `From<bool>` have been implemented for `Value<'_>`.

## v0.2.1

This release contains no code changes. The `Cargo.toml` section for docs.rs has
been updated in an attempt to enable `doc_auto_cfg`, which annotates what
features flags are needed for any given item in the documentation.

## v0.2.0

## Breaking Changes

- `Value`, `Object`, `JsonNumber`, and `JsonString` have all changed from having
  a generic parameter to having a single lifetime parameter. Internally, all
  strings have been changed to `Cow<'_, str>` to allow for owned values to be
  injected into a `Value`/`Object` or `Vec<Value<'_>>`.
- `JsonString::source` has been removed from the public API.
  `JsonString::contents()` is now available to provide read-only access to the
  JSON-encoded representation.
- `JsonNumber::source` has been removed from the public API.
  `JsonNumber::source()` is now available to provide read-only access to the
  JSON-encoded representation.
- `JsonString` now internally can be a JSON-encoded string or a raw string, and
  it will encode or decode escape sequences on the fly as needed. This means
  adding a new raw &str that needs escaping will not actually incur extra
  allocations while rendering a Value back to JSON, as the encoding operation
  will occur during the write to the destination.
- `Object::push()` has been removed, and its `Deref` implementation now targets
  `Vec<Entry<'a>>`. This allows all `Vec` methods to be used to access the
  contents of an Object, including its own `push()` function.
- `ParseDelegate::Error` is a new associated type that is now able to be
  returned from most `ParseDelegate` functions as the Error type of the Result.
  Previously, most functions did not return a Result.

  Returning an error from a `ParseDelegate` function will stop parsing the
  underlying document immediately and return the error in
  `ErrorKind::ErrorFromDelegate`.

## Fixes

- Escaped UTF16 surrogate pairs are properly handled. Previously, all surrogates
  were considered invalid unicode.
- Leading `+` signs are no longer allowed on numbers.
- Leading `0`s are now disallowed on negative numbers.

## Added

- `JsonString::from(&str)` has been added to convert from a Rust string to its
  JSON-encoded form.
- This crate now is `no_std` compatible. The `no_std` support is controlled by
  three feature flags:

  - `std`: Enabled by default, enables `alloc` automatically.
  - `alloc`: Uses the `alloc` crate for the `Document` and `Value` types.
  - `heapless`: Exports a new `HeaplessDocument` type that can be used in
    environments without `alloc`.

  Without any features enabled, only `Parser`, `GenericDocument`, `JsonString`,
  and `JsonNumber` are available to parse JSON.

  To enable `GenericDocument` to work with types other than `Vec` and
  `heapless::Vec`, `NodeCollection` can be implemented for other Vec-like types.

## Changed

- `thiserror` has been removed from the dependencies in favor of manually
  implementing `std::error::Error`. This makes `justjson` dependency free
  without enabling extra feature flags.

## v0.1.1

### Fixed

- Fixed a panic when reporting an error during decoding of some invalid unicode
  escape sequences.

## v0.1.0

- Initial release
