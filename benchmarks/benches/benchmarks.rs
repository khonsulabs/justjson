use criterion::{
    black_box, criterion_group, criterion_main, measurement::WallTime, BenchmarkGroup, Criterion,
};

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

fn yeahson_parse(json: &str) -> yeahson::Value<&str> {
    yeahson::Value::from_json(json).unwrap()
}

fn serde_json_value_parse(json: &str) -> serde_json::Value {
    serde_json::from_str(json).unwrap()
}

fn yeahson_parse_bytes(json: &str) -> yeahson::Value<&str> {
    yeahson::Value::from_json_bytes(json.as_bytes()).unwrap()
}

fn serde_json_value_parse_bytes(json: &str) -> serde_json::Value {
    serde_json::from_slice(json.as_bytes()).unwrap()
}

fn tinyjson_parse(json: &str) -> tinyjson::JsonValue {
    json.parse().unwrap()
}

fn yeahson_parse_reader(json: &str) -> yeahson::Value<String> {
    yeahson::Value::from_reader(json.as_bytes()).unwrap()
}

fn serde_json_value_parse_reader(json: &str) -> serde_json::Value {
    serde_json::from_reader(json.as_bytes()).unwrap()
}

fn bench_with_input(mut group: BenchmarkGroup<'_, WallTime>, input: &str) {
    group.bench_function("yeahson/str", |b| {
        b.iter(|| yeahson_parse(black_box(input)));
    });

    group.bench_function("yeahson/bytes", |b| {
        b.iter(|| yeahson_parse_bytes(black_box(input)));
    });

    group.bench_function("yeahson/Read", |b| {
        b.iter(|| yeahson_parse_reader(black_box(input)));
    });

    group.bench_function("serde-json/str", |b| {
        b.iter(|| serde_json_value_parse(black_box(input)));
    });

    group.bench_function("serde-json/bytes", |b| {
        b.iter(|| serde_json_value_parse_bytes(black_box(input)));
    });

    group.bench_function("serde-json/Read", |b| {
        b.iter(|| serde_json_value_parse_reader(black_box(input)));
    });

    group.bench_function("tinyjson/str", |b| {
        b.iter(|| tinyjson_parse(black_box(input)));
    });
}

fn benchmarks(c: &mut Criterion) {
    bench_with_input(c.benchmark_group("small-pretty"), SMALL_OBJECT);
    bench_with_input(c.benchmark_group("small"), SMALL_OBJECT_COMPACT);
}

criterion_group!(benches, benchmarks);
criterion_main!(benches);
