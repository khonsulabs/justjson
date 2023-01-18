# YeahSon

[![crate version](https://img.shields.io/crates/v/yeahson.svg)](https://crates.io/crates/yeahson)
[![Live Build Status](https://img.shields.io/github/actions/workflow/status/khonsulabs/yeahson/rust.yml?branch=main)](https://github.com/khonsulabs/yeahson/actions?query=workflow:Tests)
[![HTML Coverage Report for `main` branch](https://khonsulabs.github.io/yeahson/coverage/badge.svg)](https://khonsulabs.github.io/yeahson/coverage/)
[![Documentation](https://img.shields.io/badge/docs-main-informational)](https://khonsulabs.github.io/yeahson/main/yeahson)

An efficient JSON [`Value`][value] parser for Rust.

## Why another JSON parser?

This library is for developers who:

- Are working with JSON, but either don't want to or can't use `serde`.
- Want to parse a JSON Value from a slice with minimal allocations.

If neither of those situations apply to you, you should seriously consider using
`serde` and `serde-json`.

What real-world case does this use case fit? Parsing [JSON-LD Compacted Document
Form][json-ld] requires inspecting the JSON to find the `@context` field, and
using the information within the `@context` field to interpret the rest of the
document. This form of JSON-LD representation is used by
[ActivityPub][activitypub], which is a protocol that power [the
Fediverse][fediverse].

## What makes YeahSon interesting?

The optimal use case for YeahSon is when parsing an `&str` or `&[u8]`. When
parsing either of these, the returned `Value` type is `Value<&str>`. All strings
and numbers are kept in their original form in the [`JsonString`][string] and
[`JsonNumber`][number] types. While they are kept in their original form, they
are fully validated when parsed. **The net effect is significantly fewer
allocations when parsing a JSON value.**

[`JsonString`][string] implements `PartialCmp<&str>` such that if the
`JsonString` contains escape sequences, the comparison is handled correctly.
Each `JsonString` tracks its decoded length as well as whether any escapes are
present, allowing this comparison to be very efficient.

```rust
let json = yeahson::JsonString::from_json("\"Hello, World!\"").unwrap();
assert_eq!(json, "Hello, World!");
```

## Benchmarks

You can run the benchmarks by executing `cargo bench -p benchmarks`. There
currently is only a single benchmark, testing a small JSON payload in both
pretty
and compact representations.

```text
small-pretty/yeahson/str
                        time:   [704.31 ns 710.98 ns 719.96 ns]
small-pretty/yeahson/bytes
                        time:   [707.41 ns 713.11 ns 720.05 ns]
small-pretty/yeahson/Read
                        time:   [1.5964 µs 1.6050 µs 1.6160 µs]
small-pretty/serde-json/str
                        time:   [879.77 ns 884.96 ns 891.28 ns]
small-pretty/serde-json/bytes
                        time:   [1.0128 µs 1.0190 µs 1.0265 µs]
small-pretty/serde-json/Read
                        time:   [1.5034 µs 1.5091 µs 1.5172 µs]
small-pretty/tinyjson/str
                        time:   [2.4486 µs 2.4770 µs 2.5081 µs]

small/yeahson/str       time:   [676.76 ns 682.13 ns 688.31 ns]
small/yeahson/bytes     time:   [669.75 ns 674.20 ns 678.49 ns]
small/yeahson/Read      time:   [1.4983 µs 1.5020 µs 1.5065 µs]
small/serde-json/str    time:   [854.80 ns 860.16 ns 866.49 ns]
small/serde-json/bytes  time:   [953.65 ns 960.55 ns 969.05 ns]
small/serde-json/Read   time:   [1.3772 µs 1.3815 µs 1.3862 µs]
small/tinyjson/str      time:   [2.3340 µs 2.3450 µs 2.3587 µs]
```

## Why name it YeahSon?

Have you tried coming up with a JSON crate name recently? A friend jokingly
responded "YEAH SON!" when I mentioned I needed a name. It cracked me up. I
slept on it, and it still made me smile the next morning. After spending some
time trying to come up with a descriptive crate name that wasn't already taken
by another JSON parser, I eventually gave up and kept the silly name.

## Usage of Unsafe Code

This crate uses unsafe code only when converting from raw incoming data to UTF-8
data. The parser fully verifies that the data is valid UTF-8 before these
functions are used.

[value]: https://khonsulabs.github.io/yeahson/main/yeahson/enum.Value.html
[string]: https://khonsulabs.github.io/yeahson/main/yeahson/struct.JsonString.html
[number]: https://khonsulabs.github.io/yeahson/main/yeahson/struct.JsonNumber.html
[json-ld]: https://www.w3.org/TR/json-ld11/#compacted-document-form
[fediverse]: https://en.wikipedia.org/wiki/Fediverse
[activitypub]: https://www.w3.org/TR/activitypub/

## Open-source Licenses

This project, like all projects from [Khonsu Labs](https://khonsulabs.com/), are
open-source. This repository is available under the [MIT License](./LICENSE-MIT)
or the [Apache License 2.0](./LICENSE-APACHE).

To learn more about contributing, please see [CONTRIBUTING.md](./CONTRIBUTING.md).
