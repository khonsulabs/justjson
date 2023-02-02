[![crate version](https://img.shields.io/crates/v/justjson.svg)](https://crates.io/crates/justjson)
[![Live Build Status](https://img.shields.io/github/actions/workflow/status/khonsulabs/justjson/rust.yml?branch=main)](https://github.com/khonsulabs/justjson/actions?query=workflow:Tests)
[![HTML Coverage Report for `main` branch](https://khonsulabs.github.io/justjson/coverage/badge.svg)](https://khonsulabs.github.io/justjson/coverage/)
[![Documentation](https://img.shields.io/badge/docs-main-informational)]($docs-base$)

An efficient JSON [`Value`][value] crate for Rust.

**This crate is thoroughly tested, but it's still very early in development.
Feedback is greatly appreciated.**

## Why another JSON library?

This library is for developers who:

- Are working with JSON, but either don't want to or can't use `serde`.
- Want to parse a JSON Value from a slice with minimal allocations.

If neither of those situations apply to you, you should seriously consider using
`serde` and [`serde-json`][serde-json] or [`simd-json`][simd-json].

What real-world case does this use case fit? Parsing [JSON-LD Compacted Document
Form][json-ld] requires inspecting the JSON to find the `@context` field, and
using the information within the `@context` field to interpret the rest of the
document. This form of JSON-LD representation is used by
[ActivityPub][activitypub], which is a protocol that power [the
Fediverse][fediverse].

## What makes JustJson interesting?

The optimal use case for JustJson is when parsing an `&str` or `&[u8]`. When
parsing either of these, the returned [`Value`][value] type is `Value<&str>`. All strings
and numbers are kept in their original form in the [`JsonString`][string] and
[`JsonNumber`][number] types. While they are kept in their original form, they
are fully validated when parsed. **The net effect is significantly fewer
allocations when parsing a JSON value.**

[`JsonString`][string] implements `PartialEq<&str>`/`PartialCmp<&str>` such that
if the `JsonString` contains escape sequences, the comparison is handled
correctly. Each `JsonString` tracks its decoded length as well as whether any
escapes are present, allowing this comparison to be very efficient.

```rust
let json = justjson::JsonString::from_json("\"Hello, World!\"").unwrap();
assert_eq!(json, "Hello, World!");
```

JustJson also offers an even faster method for parsing: [`Document`][document].
When parsing an array or an object into a `Value`, the parser doesn't know how
large each array or object will be until it parses the rest of the object.
`Document` builds a tree representing the JSON value in a single `Vec`, further
reducing the number of allocations needed when parsing a document.

This extra speed comes at the expense of an API that requires iteration to
inspect the `Document`.

## Benchmarks

You can run the benchmarks by executing `RUSTFLAGS="-C target-cpu=native" cargo
bench -p benchmarks --all-features`. There currently is only a single benchmark,
testing a small JSON payload in both pretty and compact representations.

```text
small-pretty/justjson/str
                        time:   [802.34 ns 804.91 ns 807.35 ns]
small-pretty/justjson/doc/str
                        time:   [603.15 ns 607.13 ns 611.21 ns]
small-pretty/justjson/bytes
                        time:   [774.45 ns 775.84 ns 777.37 ns]
small-pretty/justjson/doc/bytes
                        time:   [688.51 ns 696.55 ns 705.09 ns]
small-pretty/serde-json/str
                        time:   [860.82 ns 862.12 ns 863.43 ns]
small-pretty/serde-json/bytes
                        time:   [917.49 ns 920.17 ns 923.26 ns]
small-pretty/simd-json/bytes
                        time:   [702.67 ns 704.71 ns 706.61 ns]
small-pretty/json-deserializer/bytes
                        time:   [858.81 ns 861.37 ns 864.88 ns]

small/justjson/str      time:   [736.80 ns 739.94 ns 743.56 ns]
small/justjson/doc/str  time:   [611.54 ns 618.64 ns 625.15 ns]
small/justjson/bytes    time:   [710.26 ns 711.53 ns 712.86 ns]
small/justjson/doc/bytes
                        time:   [588.97 ns 594.66 ns 600.98 ns]
small/serde-json/str    time:   [831.89 ns 833.17 ns 834.69 ns]
small/serde-json/bytes  time:   [874.84 ns 876.26 ns 877.90 ns]
small/simd-json/bytes   time:   [690.41 ns 691.48 ns 692.79 ns]
small/json-deserializer/bytes
                        time:   [778.87 ns 779.85 ns 780.85 ns]
```

## Usage of Unsafe Code

This crate uses unsafe code only when converting from raw incoming data to UTF-8
data. The parser fully verifies that the data is valid UTF-8 before these
functions are used.

## `no_std` support

By default, this crate enables the `std` feature, which adds support for Rust's
standard library types. By disabling default features, this crate can be used in
`no_std` projects. For example, in your Cargo.toml:

```toml
[dependencies]
justjson = { version = "*", default-features = false }
```

The [`Value`][value] type requires the `alloc` feature to be enabled.

The [`Document`][document] type alias requires the `alloc` feature, but the
[`GenericDocument`][generic-doc] type allows providing your own collection type.

With the `heapless` feature enabled, the [`HeaplessDocument`][heapless-doc] type
can be used to parse documents using the [heapless][heapless] crate's `Vec` type
for storage. This enables parsing JSON in environments where the `alloc` crate
isn't supported.

[value]: $value$
[string]: $string$
[number]: $number$
[document]: $document$
[generic-doc]: $generic-document$
[heapless-doc]: $heapless-document$
[json-ld]: https://www.w3.org/TR/json-ld11/#compacted-document-form
[fediverse]: https://en.wikipedia.org/wiki/Fediverse
[activitypub]: https://www.w3.org/TR/activitypub/
[simd-json]: https://github.com/simd-lite/simd-json
[serde-json]: https://github.com/serde-rs/json
[heapless]: https://github.com/japaric/heapless
