use justjson::doc::{HeaplessDocument, Node};
use justjson::{ErrorKind, JsonString};

fn main() {
    // Using a heapless vec, we can parse directly to the stack.
    let doc: HeaplessDocument<'_, 3> =
        HeaplessDocument::from_json(r#"{"hello": "world"}"#).expect("invalid json");
    let mut nodes = doc.into_iter();
    assert_eq!(nodes.next(), Some(Node::Object { length: 1 }));
    assert_eq!(nodes.next(), Some(Node::String(JsonString::from("hello"))));
    assert_eq!(nodes.next(), Some(Node::String(JsonString::from("world"))));

    // When parsing a document too large for the heapless Vec, an error will be
    // returned instead of panicing.
    let error = HeaplessDocument::<3>::from_json("[1, 2, 3, 4]").expect_err("shouldn't have space");
    assert_eq!(error.kind(), &ErrorKind::PaylodTooLarge);
}

#[test]
fn runs() {
    main();
}
