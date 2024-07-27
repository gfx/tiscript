use std::{collections::HashMap, path::Path};

use serde::{Deserialize, Serialize};
use tiscript::{from_file, from_path, from_str};

#[test]
fn test_from_file() -> Result<(), Box<dyn std::error::Error>> {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "camelCase")]
    struct EditorConfig {
        tab_size: i32,
        trim_trailing_whitespace: bool,
        end_of_line: String,
        encoding: String,
    }

    let editor_config: EditorConfig = from_file("./spec/readme.ts")?;

    assert_eq!(editor_config.tab_size, 4);
    assert_eq!(editor_config.trim_trailing_whitespace, true);
    assert_eq!(editor_config.end_of_line, "\x0a");
    assert_eq!(editor_config.encoding, "utf-8");

    Ok(())
}

#[test]
fn test_from_path() -> Result<(), Box<dyn std::error::Error>> {
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "camelCase")]
    struct EditorConfig {
        tab_size: i32,
        trim_trailing_whitespace: bool,
        end_of_line: String,
        encoding: String,
    }

    let editor_config: EditorConfig = from_path(Path::new("./spec/readme.ts"))?;

    assert_eq!(editor_config.tab_size, 4);
    assert_eq!(editor_config.trim_trailing_whitespace, true);
    assert_eq!(editor_config.end_of_line, "\x0a");
    assert_eq!(editor_config.encoding, "utf-8");

    Ok(())
}

#[test]
fn test_deserialize_scalars() -> Result<(), Box<dyn std::error::Error>> {
    #[derive(serde::Deserialize, PartialEq, Debug)]
    struct Test {
        foo: String,
        bar: bool,
        baz: f64,
        bax: i64,
    }

    let value: Test = from_str(
        r#"
        export const foo = "hello";
        export const bar = true;
        export const baz = 3.14;
        export const bax = 42;
    "#,
    )
    .unwrap();
    assert_eq!(value.foo, "hello");
    assert_eq!(value.bar, true);
    assert_eq!(value.baz, 3.14);
    assert_eq!(value.bax, 42);

    Ok(())
}

#[test]
fn test_deserialize_objects() -> Result<(), Box<dyn std::error::Error>> {
    #[derive(serde::Deserialize, PartialEq, Debug)]
    struct Test {
        foo: Vec<String>,
        bar: HashMap<String, String>,
    }

    let value: Test = from_str(
        r#"
        export const foo = ["hello", "world"];
        export const bar = { "a": "b", "x": "y" };
    "#,
    )?;

    assert_eq!(value.foo, vec!["hello", "world"]);
    assert_eq!(value.bar.get("a"), Some(&"b".to_string()));
    assert_eq!(value.bar.get("x"), Some(&"y".to_string()));

    Ok(())
}

#[test]
fn test_deserialize_option() {
    #[derive(serde::Deserialize, PartialEq, Debug)]
    struct Test {
        foo: Option<String>,
        bar: Option<String>,
    }

    let value: Test = from_str(
        r#"
        export const foo = "hello";
    "#,
    )
    .unwrap();

    assert_eq!(value.foo, Some("hello".to_string()));
    assert_eq!(value.bar, None);
}

#[test]
fn test_deserialize_option_with_explicit_nullish() {
    #[derive(serde::Deserialize, PartialEq, Debug)]
    struct Test {
        foo: Option<String>,
        bar: Option<String>,
    }

    let value: Test = from_str(
        r#"
        export const foo = undefined;
        export const bar = null;
    "#,
    )
    .unwrap();

    assert_eq!(value.foo, None);
    assert_eq!(value.bar, None);
}
