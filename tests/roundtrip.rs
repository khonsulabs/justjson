use std::fs;

use justjson::{doc::Document, Value};

#[test]
fn suite() {
    for entry in fs::read_dir("./tests/roundtrip/").unwrap() {
        let entry = entry.unwrap();
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue; };
        if name.ends_with(".json") {
            println!("Testing {name}");
            let contents = fs::read(entry.path()).unwrap();
            let value = Value::from_json_bytes(&contents).unwrap();
            let as_json = value.to_json();
            assert_eq!(as_json.as_bytes(), contents);

            // Test that Document -> Value conversion works for this value as
            // well.
            let doc = Document::from_json_bytes(&contents).unwrap();
            assert_eq!(Value::from(doc), value);
        }
    }
}
