use crate::{Error, ErrorKind, JsonNumber, JsonString, JsonStringInfo, Object, Value};

#[track_caller]
fn test_event_sequence_slice(source: &[u8], value: Value<&str>) {
    println!("Testing slice {}", std::str::from_utf8(source).unwrap());

    assert_eq!(Value::from_json_bytes(source).unwrap(), value);
}

#[track_caller]
fn test_event_sequence_read(source: &[u8], value: Value<&str>) {
    println!("Testing read {}", std::str::from_utf8(source).unwrap());

    assert_eq!(Value::from_reader(source).unwrap(), value);
}

macro_rules! test_event_sequence {
    ($src:expr, $sequence:expr) => {{
        test_event_sequence_slice($src, $sequence);
        test_event_sequence_read($src, $sequence);
    }};
    ($src:expr, $sequence:expr ,) => {{
        test_event_sequence!($src, $sequence);
    }};
}

#[test]
fn keywords() {
    test_event_sequence!(b"true", Value::Boolean(true));
    test_event_sequence!(b"false", Value::Boolean(false));
    test_event_sequence!(b"null", Value::Null);
}

#[test]
fn empty_array() {
    test_event_sequence!(b"[]", Value::Array(vec![]));
}

#[test]
fn one_element_array() {
    test_event_sequence!(b"[true]", Value::Array(vec![Value::Boolean(true)]));
}

#[test]
fn two_element_array() {
    test_event_sequence!(
        b"[true,false]",
        Value::Array(vec![Value::Boolean(true), Value::Boolean(false)])
    );
}

#[test]
fn spaced_out_array() {
    test_event_sequence!(
        b" [ true , false ] ",
        Value::Array(vec![Value::Boolean(true), Value::Boolean(false)])
    );
}

#[test]
fn whitespace() {
    test_event_sequence!(b" \t\n\rnull", Value::Null);
}

#[test]
fn basic_string() {
    test_event_sequence!(
        br#""hello""#,
        Value::String(JsonString {
            source: r#""hello""#,
            info: JsonStringInfo::new(false, 5),
        }),
    );
    test_event_sequence!(
        br#""""#,
        Value::String(JsonString {
            source: r#""""#,
            info: JsonStringInfo::new(false, 0),
        }),
    );
}

#[test]
fn escapey_string() {
    test_event_sequence!(
        br#""\"\\\/\b\f\n\r\t\u25eF""#,
        Value::String(JsonString {
            source: r#""\"\\\/\b\f\n\r\t\u25eF""#,
            info: JsonStringInfo::new(true, 11),
        }),
    )
    // TODO test decoded length
}

#[test]
fn empty_object() {
    test_event_sequence!(b"{}", Value::Object(Object::new()))
}

#[test]
fn one_mapping() {
    test_event_sequence!(
        br#"{"hello":true}"#,
        Value::Object(Object::from_iter([(
            JsonString {
                source: r#""hello""#,
                info: JsonStringInfo::new(false, 5),
            },
            Value::Boolean(true)
        )]))
    )
}

#[test]
fn two_mappings() {
    test_event_sequence!(
        br#"{"hello":true,"world":null}"#,
        Value::Object(Object::from_iter([
            (
                JsonString {
                    source: r#""hello""#,
                    info: JsonStringInfo::new(false, 5),
                },
                Value::Boolean(true)
            ),
            (
                JsonString {
                    source: r#""world""#,
                    info: JsonStringInfo::new(false, 5),
                },
                Value::Null
            )
        ]))
    )
}

#[test]
fn spaced_out_object() {
    test_event_sequence!(
        br#" { "hello" : true , "world" : null } "#,
        Value::Object(Object::from_iter([
            (
                JsonString {
                    source: r#""hello""#,
                    info: JsonStringInfo::new(false, 5),
                },
                Value::Boolean(true)
            ),
            (
                JsonString {
                    source: r#""world""#,
                    info: JsonStringInfo::new(false, 5),
                },
                Value::Null
            )
        ]))
    )
}

#[test]
fn numbers() {
    for b in b'0'..=b'9' {
        test_event_sequence!(
            &[b],
            Value::Number(JsonNumber {
                source: std::str::from_utf8(&[b]).unwrap(),
            }),
        );
    }

    test_event_sequence!(br#"-1"#, Value::Number(JsonNumber { source: r#"-1"# }),);
    test_event_sequence!(br#"+01"#, Value::Number(JsonNumber { source: r#"+01"# }));

    test_event_sequence!(br#"-1.0"#, Value::Number(JsonNumber { source: r#"-1.0"# }),);

    test_event_sequence!(
        br#"-1.0e1"#,
        Value::Number(JsonNumber {
            source: r#"-1.0e1"#,
        }),
    );

    test_event_sequence!(
        br#"-1.0E-1"#,
        Value::Number(JsonNumber {
            source: r#"-1.0E-1"#,
        }),
    );
}

#[test]
fn object_of_everything() {
    test_event_sequence!(
        br#"{"a":1,"b":true,"c":"hello","d":[],"e":{}}"#,
        Value::Object(Object::from_iter([
            (
                JsonString::from_json(r#""a""#).unwrap(),
                Value::Number(JsonNumber { source: r#"1"# })
            ),
            (
                JsonString::from_json(r#""b""#).unwrap(),
                Value::Boolean(true)
            ),
            (
                JsonString::from_json(r#""c""#).unwrap(),
                Value::String(JsonString::from_json(r#""hello""#).unwrap())
            ),
            (
                JsonString::from_json(r#""d""#).unwrap(),
                Value::Array(vec![])
            ),
            (
                JsonString::from_json(r#""e""#).unwrap(),
                Value::Object(Object::new())
            ),
        ]))
    );
}

#[test]
fn array_of_everything() {
    test_event_sequence!(
        br#"[1,true,"hello",[],{}]"#,
        Value::Array(vec![
            Value::Number(JsonNumber { source: r#"1"# }),
            Value::Boolean(true),
            Value::String(JsonString::from_json(r#""hello""#).unwrap()),
            Value::Array(vec![]),
            Value::Object(Object::new())
        ])
    );
}

#[track_caller]
fn expect_json_error(json: &str) -> Error {
    println!("Parsing {json}");
    let err = Value::from_json(json).expect_err("parsing did not error");
    println!("> {err:?}");
    err
}

#[track_caller]
fn expect_json_error_reader(json: &str) -> Error {
    println!("Parsing via Read {json}");
    let err = Value::from_reader(json.as_bytes()).expect_err("parsing did not error");
    println!("> {err:?}");
    err
}

macro_rules! assert_json_error_kind_matches {
    ($json:expr, $offset:expr, $kind:expr) => {{
        let err = expect_json_error($json);
        assert_eq!(
            err,
            Error {
                offset: $offset,
                kind: $kind
            }
        );

        let err = expect_json_error_reader($json);
        assert_eq!(
            err,
            Error {
                offset: $offset,
                kind: $kind
            }
        );
    }};
}

#[test]
fn object_errors() {
    assert_json_error_kind_matches!(r#"{1:true}"#, 1, ErrorKind::ObjectKeysMustBeStrings);
    assert_json_error_kind_matches!(r#"{"a": true,}"#, 11, ErrorKind::ExpectedObjectKey);
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

    assert_json_error_kind_matches!(
        r#""\udddd"#,
        2,
        ErrorKind::from(std::str::from_utf8(b"\xdd\xdd").expect_err("invalid codepoint"))
    );

    assert_json_error_kind_matches!(r#""\uG"#, 3, ErrorKind::InvalidHexadecimal);
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
