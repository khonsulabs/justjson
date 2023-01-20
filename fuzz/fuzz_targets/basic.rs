#![no_main]

use libfuzzer_sys::fuzz_target;

use justjson::Value;

fuzz_target!(|data: &str| {
    let _ = Value::from_json(data);
});
