use criterion::measurement::WallTime;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkGroup, Criterion};

const SMALL_OBJECT: &str = r#"{
    "@context": "https://www.w3.org/ns/activitystreams",
    "summary": "A note",
    "type": "Note",
    "content": "My dog has fleas.",
    "numbers": [1, 2, 4.4],
    "keywords": {
        "true": true,
        "false": false,
        "null": null
    }
}"#;

const SMALL_OBJECT_COMPACT: &str = r#"{"@context":"https://www.w3.org/ns/activitystreams","summary":"A note","type":"Note","content":"My dog has fleas.","numbers":[1,2,4.4],"keywords":{"true":true,"false":false,"null":null}}"#;

fn justjson_parse(json: &str) -> justjson::Value<&str> {
    justjson::Value::from_json(json).unwrap()
}

fn justjson_parse_doc(json: &str) -> justjson::doc::Document<'_> {
    justjson::doc::Document::from_json(json).unwrap()
}

fn serde_json_value_parse(json: &str) -> serde_json::Value {
    serde_json::from_str(json).unwrap()
}

fn json_deserializer_parse_bytes(json: &str) -> json_deserializer::Value {
    json_deserializer::parse(json.as_bytes()).unwrap()
}

#[cfg(feature = "simd-json")]
fn simd_json_parse(json: &mut Vec<u8>) {
    let _ = simd_json::to_borrowed_value(json).unwrap();
}

fn justjson_parse_bytes(json: &str) -> justjson::Value<&str> {
    justjson::Value::from_json_bytes(json.as_bytes()).unwrap()
}

fn justjson_parse_doc_bytes(json: &str) -> justjson::doc::Document<'_> {
    justjson::doc::Document::from_json_bytes(json.as_bytes()).unwrap()
}

fn serde_json_value_parse_bytes(json: &str) -> serde_json::Value {
    serde_json::from_slice(json.as_bytes()).unwrap()
}

fn bench_with_input(mut group: BenchmarkGroup<'_, WallTime>, input: &str) {
    group.bench_function("justjson/str", |b| {
        b.iter(|| justjson_parse(black_box(input)));
    });

    group.bench_function("justjson/doc/str", |b| {
        b.iter(|| justjson_parse_doc(black_box(input)));
    });

    group.bench_function("justjson/bytes", |b| {
        b.iter(|| justjson_parse_bytes(black_box(input)));
    });

    group.bench_function("justjson/doc/bytes", |b| {
        b.iter(|| justjson_parse_doc_bytes(black_box(input)));
    });

    group.bench_function("serde-json/str", |b| {
        b.iter(|| serde_json_value_parse(black_box(input)));
    });

    group.bench_function("serde-json/bytes", |b| {
        b.iter(|| serde_json_value_parse_bytes(black_box(input)));
    });

    #[cfg(feature = "simd-json")]
    group.bench_function("simd-json/bytes", |b| {
        let mut bytes = input.as_bytes().to_vec();
        b.iter(|| simd_json_parse(black_box(&mut bytes)));
    });

    group.bench_function("json-deserializer/bytes", |b| {
        b.iter(|| json_deserializer_parse_bytes(black_box(input)));
    });
}

fn benchmarks(c: &mut Criterion) {
    bench_with_input(c.benchmark_group("small-pretty"), SMALL_OBJECT);
    bench_with_input(c.benchmark_group("small"), SMALL_OBJECT_COMPACT);
}

criterion_group!(benches, benchmarks);
criterion_main!(benches);
