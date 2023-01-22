# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

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

## Changed

- `thiserror` has been removed from the dependencies in favor of manually
  implementing `std::error::Error`. This makes `justjson` dependency free (for
  now).

## v0.1.1

### Fixed

- Fixed a panic when reporting an error during decoding of some invalid unicode
  escape sequences.

## v0.1.0

- Initial release
