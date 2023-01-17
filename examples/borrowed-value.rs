const SMALL_OBJECT: &str = r#"{
    "@context": "https://www.w3.org/ns/activitystreams",
    "summary": "A note",
    "type": "Note",
    "content": "My dog has fleas.",
    "numbers": [1, 2, 4.4]
}"#;

fn yeahson_parse(json: &str) -> yeahson::Value<&str> {
    yeahson::Value::from_json(json).unwrap()
}

fn main() {
    const ITERS: usize = 1000000;
    let mut vec = Vec::with_capacity(ITERS);
    for _ in 0..ITERS {
        vec.push(yeahson_parse(SMALL_OBJECT));
    }
}
