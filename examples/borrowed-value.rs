const SMALL_OBJECT: &str = r#"{
    "@context": "https://www.w3.org/ns/activitystreams",
    "summary": "A note",
    "type": "Note",
    "content": "My dog has fleas.",
    "numbers": [1, 2, 4.4]
}"#;

fn justjson_parse(json: &str) -> justjson::Value<&str> {
    justjson::Value::from_json(json).unwrap()
}

fn main() {
    // TODO this isn't actually an example... it was a way to get criterion out
    // of the way for profiling.
    const ITERS: usize = 100;
    for _ in 0..ITERS {
        justjson_parse(include_str!("../../json-benchmark/data/canada.json"));
    }
}

#[test]
fn runs() {
    main();
}
