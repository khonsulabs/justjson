use std::fs;

use justjson::{doc::Document, parser::ParseConfig, Value};

#[test]
fn suite() {
    for entry in fs::read_dir("./tests/jsonorg/").unwrap() {
        let entry = entry.unwrap();
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue; };
        let config = ParseConfig::strict().with_recursion_limit(19);
        if name.ends_with(".json") {
            println!("Testing {name}");
            let contents = fs::read(entry.path()).unwrap();
            if name.starts_with("pass") {
                Value::from_json_bytes_with_config(&contents, config)
                    .expect("failed to parse success case");
                Document::from_json_bytes_with_config(&contents, config)
                    .expect("failed to parse success case");
            } else {
                Value::from_json_bytes_with_config(&contents, config)
                    .expect_err("success on failure case");
                Document::from_json_bytes_with_config(&contents, config)
                    .expect_err("success on failure case");
            }
        }
    }
}
