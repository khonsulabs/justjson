use justjson::Value;

fn main() {
    let value = Value::from_json(r#"{"hello":"world"}"#).expect("error parsing json");
    let obj = value.as_object().expect("json contains an object");
    assert_eq!(obj[0].key, "hello");
    let value = obj[0].value.as_string().expect("value is a string");
    assert_eq!(value, "world");
}

#[test]
fn runs() {
    main();
}
