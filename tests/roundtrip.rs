use std::fs;

use yeahson::Value;

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
            assert_eq!(as_json, String::from_utf8(contents).unwrap());
        }
    }
}
