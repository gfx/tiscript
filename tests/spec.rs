use std::{fs, path::Path};

use serde_json::json;
use titys::{util::eval, value::Value};

#[derive(Debug)]
struct Spec {
    filename: String,
    is_err: bool,
    expected_stdout: String,
    expected_stderr: String,
}

fn collect_specs(spec_dir: &Path) -> Vec<Spec> {
    let mut specs = vec![];

    for entry in fs::read_dir(spec_dir).unwrap() {
        let path = entry.unwrap().path();

        if path.is_file() {
            let filename = path.file_name().unwrap().to_str().unwrap();
            if filename.ends_with(".ts") {
                let mut expected_stdout = String::new(); // $filename.stdout
                let mut expected_stderr = String::new(); // $filename.stderr

                if let Ok(expected) =
                    fs::read_to_string(spec_dir.join(format!("{}.stdout", filename)))
                {
                    expected_stdout = expected;
                }
                if let Ok(expected) =
                    fs::read_to_string(spec_dir.join(format!("{}.stderr", filename)))
                {
                    expected_stderr = expected;
                }
                let is_err = filename.starts_with("err.");

                specs.push(Spec {
                    filename: path.to_string_lossy().to_string(),
                    is_err,
                    expected_stdout,
                    expected_stderr,
                });
            }
        }
    }

    specs
}

#[test]
fn test_evaluate_specs() {
    // UPDATE=1 to update expected files
    let update = std::env::var("UPDATE").is_ok();

    let specs = collect_specs(Path::new("spec"));

    for spec in specs {
        let source = fs::read_to_string(&spec.filename).unwrap();
        let result = eval(&source, Path::new(&spec.filename));
        let mut json = serde_json::Map::new();

        if spec.is_err {
            assert!(result.is_err());
            let err = result.unwrap_err();

            if update {
                fs::write(
                    format!("{}.stderr", spec.filename),
                    err.to_string().as_bytes(),
                )
                .unwrap();
            } else {
                assert_eq!(err.to_string(), spec.expected_stderr);
            }
        } else {
            let exports = result.unwrap();
            for (name, value) in exports.into_iter() {
                match value {
                    Value::Null => {
                        json.insert(name, serde_json::Value::Null);
                    }
                    Value::Int(v) => {
                        json.insert(name, json!(v));
                    }
                    Value::Num(v) => {
                        if v.fract() == 0.0 {
                            json.insert(name, json!(v as i64));
                        } else {
                            json.insert(name, json!(v));
                        }
                    }
                    Value::Str(v) => {
                        json.insert(name, json!(v));
                    }
                    _ => {
                        panic!("Cannot export the value: {:?}", value);
                    }
                }
            }

            if update {
                fs::write(
                    format!("{}.stdout", spec.filename),
                    serde_json::to_string_pretty(&json).unwrap().as_bytes(),
                )
                .unwrap();
            } else {
                assert_eq!(
                    serde_json::to_string_pretty(&json).unwrap(),
                    spec.expected_stdout
                );
            }
        }
    }
}
