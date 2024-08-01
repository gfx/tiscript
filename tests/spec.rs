// UPDATE=1 to update expected files

use std::{fs, path::Path};

use pretty_assertions::assert_eq;

use tiscript::util::{eval, EvalOptions};

#[derive(Debug)]
struct Spec {
    filename: String,
    is_err: bool,
    expected_stdout: Option<String>,
    expected_stderr: Option<String>,
}

fn run_tiscript2json(filename: &String) -> Result<String, Box<dyn std::error::Error>> {
    // run `node tiscript2json $filename`
    let output = std::process::Command::new("node")
        .arg("tiscript2json")
        .arg(filename)
        .output()?;

    if output.status.success() {
        Ok(String::from_utf8(output.stdout)?)
    } else {
        Err(String::from_utf8(output.stderr)?.into())
    }
}

fn collect_specs(spec_dir: &Path) -> Vec<Spec> {
    let mut specs = vec![];

    for entry in fs::read_dir(spec_dir).unwrap() {
        let path = entry.unwrap().path();

        if path.is_file() {
            let filename = path.file_name().unwrap().to_str().unwrap();
            if filename.ends_with(".ts") || filename.ends_with(".mts") {
                let mut expected_stdout: Option<String> = None; // $filename.stdout
                let mut expected_stderr: Option<String> = None; // $filename.stderr

                if let Ok(expected) =
                    fs::read_to_string(spec_dir.join(format!("{}.stdout", filename)))
                {
                    expected_stdout = Some(expected);
                }
                if let Ok(expected) =
                    fs::read_to_string(spec_dir.join(format!("{}.stderr", filename)))
                {
                    expected_stderr = Some(expected);
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
fn test_evaluate_specs_that_should_pass() -> Result<(), Box<dyn std::error::Error>> {
    let update = std::env::var("UPDATE").is_ok();

    let specs = collect_specs(Path::new("spec"));

    for spec in specs {
        if spec.is_err {
            continue;
        }
        let source = fs::read_to_string(&spec.filename)?;
        let result = eval(&source, &EvalOptions::new_from_path_str(&spec.filename));
        let tiscript2json = run_tiscript2json(&spec.filename);

        assert!(result.is_ok(), "tiscript should pass in {}", spec.filename);
        assert!(
            tiscript2json.is_ok(),
            "tiscript2json should pass in {}",
            spec.filename
        );

        let output = serde_json::to_string_pretty(&result?)? + "\n";
        let tiscript2json: serde_json::Value = serde_json::from_str(&tiscript2json?)?;

        assert_eq!(
            output,
            serde_json::to_string_pretty(&tiscript2json)? + "\n",
            "output vs tsc in {}",
            spec.filename
        );

        if update {
            fs::write(format!("{}.stdout", spec.filename), output.as_bytes())?;
        } else {
            assert_eq!(
                output,
                spec.expected_stdout.unwrap_or_default(),
                "output vs expected in {}",
                spec.filename
            );
        }
    }

    Ok(())
}

#[test]
fn test_evaluate_specs_that_should_fail() -> Result<(), Box<dyn std::error::Error>> {
    // UPDATE=1 to update expected files
    let update = std::env::var("UPDATE").is_ok();

    let specs = collect_specs(Path::new("spec"));

    for spec in specs {
        if !spec.is_err {
            continue;
        }
        let source = fs::read_to_string(&spec.filename)?;
        let result = eval(&source, &EvalOptions::new_from_path_str(&spec.filename));

        assert!(result.is_err());
        let err = result.unwrap_err();

        let tiscript_full_message = err.to_string();
        let (_, err) = tiscript_full_message.split_at(tiscript_full_message.find(' ').unwrap() + 1);

        if update {
            fs::write(
                format!("{}.stderr", spec.filename),
                err.to_string().as_bytes(),
            )?;
        } else {
            assert_eq!(
                err.to_string().trim(),
                spec.expected_stderr.unwrap_or_default().trim(),
                "output vs expected in {}",
                spec.filename
            );
        }
    }

    Ok(())
}
