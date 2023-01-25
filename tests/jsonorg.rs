use justjson::parser::{ParseConfig, Parser};
#[cfg(feature = "alloc")]
use justjson::{doc::Document, Value};

macro_rules! json_org_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            let src = include_bytes!(concat!("./jsonorg/", stringify!($name), ".json"));
            let src_str = std::str::from_utf8(src).unwrap();
            let config = ParseConfig::strict().with_recursion_limit(19);

            if stringify!($name).starts_with("pass") {
                Parser::validate_json_bytes_with_config(src, config)
                    .expect("failed to parse success case");
                Parser::validate_json_with_config(src_str, config)
                    .expect("failed to parse success case");
                #[cfg(feature = "alloc")]
                {
                    Value::from_json_bytes_with_config(src, config)
                        .expect("failed to parse success case");
                    Value::from_json_with_config(src_str, config)
                        .expect("failed to parse success case");

                    Document::from_json_bytes_with_config(src, config)
                        .expect("failed to parse success case");
                    Document::from_json_with_config(src_str, config)
                        .expect("failed to parse success case");
                }
            } else {
                Parser::validate_json_bytes_with_config(src, config)
                    .expect_err("success on failure case");
                Parser::validate_json_with_config(src_str, config)
                    .expect_err("success on failure case");
                #[cfg(feature = "alloc")]
                {
                    Value::from_json_bytes_with_config(src, config)
                        .expect_err("success on failure case");
                    Value::from_json_with_config(src_str, config)
                        .expect_err("success on failure case");

                    Document::from_json_bytes_with_config(src, config)
                        .expect_err("success on failure case");
                    Document::from_json_with_config(src_str, config)
                        .expect_err("success on failure case");
                }
            }
        }
    };
}

json_org_test!(pass1);
json_org_test!(pass2);
json_org_test!(pass3);

json_org_test!(fail1);
json_org_test!(fail2);
json_org_test!(fail3);
json_org_test!(fail4);
json_org_test!(fail5);
json_org_test!(fail6);
json_org_test!(fail7);
json_org_test!(fail8);
json_org_test!(fail9);
json_org_test!(fail10);
json_org_test!(fail11);
json_org_test!(fail12);
json_org_test!(fail13);
json_org_test!(fail14);
json_org_test!(fail15);
json_org_test!(fail16);
json_org_test!(fail17);
json_org_test!(fail18);
json_org_test!(fail19);
json_org_test!(fail20);
json_org_test!(fail21);
json_org_test!(fail22);
json_org_test!(fail23);
json_org_test!(fail24);
json_org_test!(fail25);
json_org_test!(fail26);
json_org_test!(fail27);
json_org_test!(fail28);
json_org_test!(fail29);
json_org_test!(fail30);
json_org_test!(fail31);
json_org_test!(fail32);
json_org_test!(fail33);
