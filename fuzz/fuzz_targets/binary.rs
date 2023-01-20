#![no_main]

use libfuzzer_sys::fuzz_target;

use justjson::Value;

fuzz_target!(|data: &[u8]| {
    let _ = Value::from_json_bytes(data);
});
