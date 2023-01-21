use std::fs;

use justjson::parser::ParseConfig;
use justjson::Value;

#[test]
fn suite() {
    for entry in fs::read_dir("./tests/JSONTestSuite/").unwrap() {
        let entry = entry.unwrap();
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue; };
        let config = ParseConfig::default();
        if name.ends_with(".json") {
            println!("Testing {name}");
            let contents = fs::read(entry.path()).unwrap();
            let result = Value::from_json_bytes_with_config(&contents, config);
            match name.chars().next().unwrap() {
                'i' => {
                    // Doesn't matter, just don't crash.
                    println!("i test result: {}", result.is_ok())
                }
                'y' => {
                    result.expect("failed when expecting success");
                }
                'n' => {
                    result.expect_err("success when expecting failure");
                }
                _ => unreachable!(),
            }
        }
    }
}
