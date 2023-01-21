use std::borrow::Cow;
use std::fmt::Write;

use crate::doc::Document;
use crate::string::StringContents;
use crate::{Error, ErrorKind, JsonNumber, JsonString, JsonStringInfo, Object, Value};

#[track_caller]
fn test_json_parse(source: &[u8], value: &Value<'_>) {
    println!("Testing slice {}", std::str::from_utf8(source).unwrap());

    let parsed_value = Value::from_json_bytes(source).unwrap();
    assert_eq!(&parsed_value, value);

    let doc = Document::from_json_bytes(source).unwrap();
    assert_eq!(Value::from(doc), parsed_value);
}

#[test]
fn keywords() {
    test_json_parse(b"true", &Value::Boolean(true));
    test_json_parse(b"false", &Value::Boolean(false));
    test_json_parse(b"null", &Value::Null);
}

#[test]
fn empty_array() {
    test_json_parse(b"[]", &Value::Array(vec![]));
}

#[test]
fn one_element_array() {
    test_json_parse(b"[true]", &Value::Array(vec![Value::Boolean(true)]));
}

#[test]
fn two_element_array() {
    test_json_parse(
        b"[true,false]",
        &Value::Array(vec![Value::Boolean(true), Value::Boolean(false)]),
    );
}

#[test]
fn spaced_out_array() {
    test_json_parse(
        b" [ true , false ] ",
        &Value::Array(vec![Value::Boolean(true), Value::Boolean(false)]),
    );
}

#[test]
fn whitespace() {
    test_json_parse(b" \t\n\rnull", &Value::Null);
}

#[test]
fn basic_string() {
    test_json_parse(
        br#""hello""#,
        &Value::String(JsonString {
            source: StringContents::Json {
                source: Cow::Borrowed("hello"),
                info: JsonStringInfo::new(false, 5),
            },
        }),
    );
    test_json_parse(
        br#""""#,
        &Value::String(JsonString {
            source: StringContents::Json {
                source: Cow::Borrowed(""),
                info: JsonStringInfo::new(false, 0),
            },
        }),
    );
}

#[test]
fn escapey_string() {
    test_json_parse(
        br#""\"\\\/\b\f\n\r\t\u25eF""#,
        &Value::String(JsonString {
            source: StringContents::Json {
                source: Cow::Borrowed(r#"\"\\\/\b\f\n\r\t\u25eF"#),
                info: JsonStringInfo::new(true, 11),
            },
        }),
    );
}

#[test]
fn empty_object() {
    test_json_parse(b"{}", &Value::Object(Object::new()));
}

#[test]
fn one_mapping() {
    test_json_parse(
        br#"{"hello":true}"#,
        &Value::Object(Object::from_iter([(
            JsonString {
                source: StringContents::Json {
                    source: Cow::Borrowed("hello"),
                    info: JsonStringInfo::new(false, 5),
                },
            },
            Value::Boolean(true),
        )])),
    );
}

#[test]
fn two_mappings() {
    test_json_parse(
        br#"{"hello":true,"world":null}"#,
        &Value::Object(Object::from_iter([
            (
                JsonString {
                    source: StringContents::Json {
                        source: Cow::Borrowed("hello"),
                        info: JsonStringInfo::new(false, 5),
                    },
                },
                Value::Boolean(true),
            ),
            (
                JsonString {
                    source: StringContents::Json {
                        source: Cow::Borrowed("world"),
                        info: JsonStringInfo::new(false, 5),
                    },
                },
                Value::Null,
            ),
        ])),
    );
}

#[test]
fn spaced_out_object() {
    test_json_parse(
        br#" { "hello" : true , "world" : null } "#,
        &Value::Object(Object::from_iter([
            (
                JsonString::from_json("\"hello\"").unwrap(),
                Value::Boolean(true),
            ),
            (JsonString::from_json("\"world\"").unwrap(), Value::Null),
        ])),
    );
}

#[test]
fn numbers() {
    for b in b'0'..=b'9' {
        test_json_parse(
            &[b],
            &Value::Number(JsonNumber {
                source: Cow::Borrowed(std::str::from_utf8(&[b]).unwrap()),
            }),
        );
    }

    test_json_parse(
        br#"-1"#,
        &Value::Number(JsonNumber {
            source: Cow::Borrowed(r#"-1"#),
        }),
    );
    test_json_parse(
        br#"+01"#,
        &Value::Number(JsonNumber {
            source: Cow::Borrowed(r#"+01"#),
        }),
    );

    test_json_parse(
        br#"-1.0"#,
        &Value::Number(JsonNumber {
            source: Cow::Borrowed(r#"-1.0"#),
        }),
    );

    test_json_parse(
        br#"-1.0e1"#,
        &Value::Number(JsonNumber {
            source: Cow::Borrowed(r#"-1.0e1"#),
        }),
    );

    test_json_parse(
        br#"-1.0E-1"#,
        &Value::Number(JsonNumber {
            source: Cow::Borrowed(r#"-1.0E-1"#),
        }),
    );
}

#[test]
fn object_of_everything() {
    test_json_parse(
        br#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#,
        &Value::Object(Object::from_iter([
            (
                JsonString::from_json(r#""a""#).unwrap(),
                Value::Number(JsonNumber {
                    source: Cow::Borrowed(r#"1"#),
                }),
            ),
            (
                JsonString::from_json(r#""b""#).unwrap(),
                Value::Boolean(true),
            ),
            (
                JsonString::from_json(r#""c""#).unwrap(),
                Value::String(JsonString::from_json(r#""hello""#).unwrap()),
            ),
            (
                JsonString::from_json(r#""d""#).unwrap(),
                Value::Array(vec![]),
            ),
            (
                JsonString::from_json(r#""e""#).unwrap(),
                Value::Object(Object::new()),
            ),
        ])),
    );
}

#[test]
fn array_of_everything() {
    test_json_parse(
        br#"[1,true,"hello",[],{}]"#,
        &Value::Array(vec![
            Value::Number(JsonNumber {
                source: Cow::Borrowed(r#"1"#),
            }),
            Value::Boolean(true),
            Value::String(JsonString::from_json(r#""hello""#).unwrap()),
            Value::Array(vec![]),
            Value::Object(Object::new()),
        ]),
    );
}

#[track_caller]
fn expect_json_error(json: &str) -> Error {
    println!("Parsing {json}");
    let err = Value::from_json(json).expect_err("parsing did not error");
    println!("> {err:?}");
    err
}

macro_rules! assert_json_error_kind_matches {
    ($json:expr, $offset:expr, $kind:expr) => {{
        let err = expect_json_error($json);
        assert_eq!(err.kind(), &$kind);
        assert_eq!(err.offset(), $offset);
    }};
}

#[test]
fn object_errors() {
    assert_json_error_kind_matches!(r#"{1:true}"#, 1, ErrorKind::ObjectKeysMustBeStrings);
    assert_json_error_kind_matches!(r#"{"a": true,}"#, 11, ErrorKind::IllegalTrailingComma);
    assert_json_error_kind_matches!(r#"{"a": true,:"#, 11, ErrorKind::ExpectedObjectKey);
    assert_json_error_kind_matches!(r#"{"a"}"#, 4, ErrorKind::ExpectedColon);
    assert_json_error_kind_matches!(r#"{"a""#, 4, ErrorKind::ExpectedColon);
    assert_json_error_kind_matches!(r#"{"a":}"#, 5, ErrorKind::Unexpected(b'}'));
    assert_json_error_kind_matches!(r#"{"a":1]"#, 6, ErrorKind::ExpectedCommaOrEndOfObject);
    assert_json_error_kind_matches!(r#"{"a": true,"#, 11, ErrorKind::UnclosedObject);
}

#[test]
fn array_errors() {
    assert_json_error_kind_matches!(r#"[1,]"#, 3, ErrorKind::IllegalTrailingComma);

    assert_json_error_kind_matches!(r#"[1}"#, 2, ErrorKind::ExpectedCommaOrEndOfArray);

    assert_json_error_kind_matches!(r#"[1,,}"#, 3, ErrorKind::Unexpected(b','));

    assert_json_error_kind_matches!(r#"[1,"#, 3, ErrorKind::UnclosedArray);

    assert_json_error_kind_matches!(r#"["#, 1, ErrorKind::UnclosedArray);
}

#[test]
fn keyword_errors() {
    assert_json_error_kind_matches!(r#"tru "#, 3, ErrorKind::Unexpected(b' '));
    assert_json_error_kind_matches!(r#"tru"#, 3, ErrorKind::UnexpectedEof);
}

#[test]
fn json_errors() {
    assert_json_error_kind_matches!(r#"true true"#, 5, ErrorKind::TrailingNonWhitespace);

    assert_json_error_kind_matches!(r#","#, 0, ErrorKind::Unexpected(b','));

    assert_json_error_kind_matches!(r#"#"#, 0, ErrorKind::Unexpected(b'#'));

    assert_json_error_kind_matches!(r#""#, 0, ErrorKind::UnexpectedEof);
}

#[test]
fn string_errors() {
    assert_json_error_kind_matches!(r#"""#, 1, ErrorKind::UnclosedString);

    assert_json_error_kind_matches!("\"\0\"", 1, ErrorKind::Unexpected(b'\0'));

    assert_json_error_kind_matches!(r#""\?"#, 2, ErrorKind::InvalidEscape);

    assert_json_error_kind_matches!(r#""\udddd"#, 2, ErrorKind::Utf8);
    assert_json_error_kind_matches!(r#""\udda1"#, 2, ErrorKind::Utf8);

    assert_json_error_kind_matches!(r#""\uG"#, 3, ErrorKind::InvalidHexadecimal);

    println!("Parsing invalid unicode");
    let err = Value::from_json_bytes(b"\"\xdd\xdd\"").expect_err("parsing did not error");
    println!("> {err:?}");
    assert!(matches!(err.kind(), ErrorKind::Utf8));
}

#[test]
fn number_errors() {
    assert_json_error_kind_matches!(r#"- "#, 1, ErrorKind::ExpectedDigit);

    assert_json_error_kind_matches!(r#"1. "#, 2, ErrorKind::ExpectedDecimalDigit);

    assert_json_error_kind_matches!(r#"1.0E "#, 4, ErrorKind::ExpectedExponent);

    assert_json_error_kind_matches!(r#"1.0E- "#, 5, ErrorKind::ExpectedExponent);

    // Same battery of tests but with an eof instead
    assert_json_error_kind_matches!(r#"-"#, 1, ErrorKind::ExpectedDigit);

    assert_json_error_kind_matches!(r#"1."#, 2, ErrorKind::ExpectedDecimalDigit);

    assert_json_error_kind_matches!(r#"1.0E"#, 4, ErrorKind::ExpectedExponent);

    assert_json_error_kind_matches!(r#"1.0E-"#, 5, ErrorKind::ExpectedExponent);
}

fn test_roundtrip_encoding(source: &str) {
    println!("Testing {source}");
    let value = Value::from_json(source).unwrap();
    assert_eq!(value.to_json(), source);
}

fn test_roundtrip_encoding_pretty_custom(source: &str, indentation: &str, line_ending: &str) {
    println!("Testing {source}");
    let value = Value::from_json(source).unwrap();
    assert_eq!(
        value.to_json_pretty_custom(indentation, line_ending),
        source
    );
}

fn test_roundtrip_encoding_pretty(source: &str) {
    println!("Testing {source}");
    let value = Value::from_json(source).unwrap();
    assert_eq!(value.to_json_pretty(), source);
}

#[test]
fn json_formatting() {
    test_roundtrip_encoding(r#"[1,true,"hello",[],{}]"#);
    test_roundtrip_encoding(r#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#);
    test_roundtrip_encoding_pretty("[\n  1,\n  true,\n  \"hello\",\n  [],\n  {}\n]");
    test_roundtrip_encoding_pretty(
        "{\n  \"a\": 1,\n  \"b\": true,\n  \"c\": \"hello\",\n  \"d\": [],\n  \"e\": {}\n}",
    );
    test_roundtrip_encoding_pretty_custom(
        "{\r\t\"a\": 1,\r\t\"b\": true,\r\t\"c\": \"hello\",\r\t\"d\": [],\r\t\"e\": {}\r}",
        "\t",
        "\r",
    );
}

#[test]
fn value_display() {
    let value = Value::from_json(r#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#).unwrap();
    assert_eq!(
        value.to_string(),
        r#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#
    );
    let mut pretty = String::new();
    write!(&mut pretty, "{value:#}").unwrap();
    assert_eq!(
        pretty,
        "{\n  \"a\": 1,\n  \"b\": true,\n  \"c\": \"hello\",\n  \"d\": [],\n  \"e\": {}\n}"
    );
}
