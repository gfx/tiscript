// UPDATE=1 to update expected files

use std::{fs, path::Path};

use pretty_assertions::assert_eq;

use titys::util::eval;

#[derive(Debug)]
struct Spec {
    filename: String,
    is_err: bool,
    expected_stdout: String,
    expected_stderr: String,
}

fn run_titys2json(filename: &String) -> Result<String, Box<dyn std::error::Error>> {
    // run `node titys2json $filename`
    let output = std::process::Command::new("node")
        .arg("titys2json")
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
fn test_evaluate_specs_that_should_pass() {
    let update = std::env::var("UPDATE").is_ok();

    let specs = collect_specs(Path::new("spec"));

    for spec in specs {
        if spec.is_err {
            continue;
        }
        let source = fs::read_to_string(&spec.filename).unwrap();
        let result = eval(&source, Path::new(&spec.filename));
        let titys2json = run_titys2json(&spec.filename);

        assert!(result.is_ok(), "titys should pass in {}", spec.filename);
        assert!(
            titys2json.is_ok(),
            "titys2json should pass in {}",
            spec.filename
        );

        let output = serde_json::ser::to_string_pretty(&result.unwrap()).unwrap() + "\n";

        assert_eq!(
            output.trim(),
            titys2json.unwrap().trim(),
            "output vs tsc in {}",
            spec.filename
        );

        if update {
            fs::write(format!("{}.stdout", spec.filename), output.as_bytes()).unwrap();
        } else {
            assert_eq!(
                output.trim(),
                spec.expected_stdout.trim(),
                "output vs expected in {}",
                spec.filename
            );
        }
    }
}

#[test]
fn test_evaluate_specs_that_should_fail() {
    // UPDATE=1 to update expected files
    let update = std::env::var("UPDATE").is_ok();

    let specs = collect_specs(Path::new("spec"));

    for spec in specs {
        if !spec.is_err {
            continue;
        }
        let source = fs::read_to_string(&spec.filename).unwrap();
        let result = eval(&source, Path::new(&spec.filename));
        let titys2json = run_titys2json(&spec.filename);

        assert!(result.is_err());
        assert!(titys2json.is_err());
        let err = result.unwrap_err();

        // extract the message body (just after the first whitespace)
        let tsc_full_message = titys2json.unwrap_err().to_string();
        let (_, tsc_message) = tsc_full_message.split_at(tsc_full_message.find(' ').unwrap() + 1);

        let titys_full_message = err.to_string();
        let (_, err) = titys_full_message.split_at(titys_full_message.find(' ').unwrap() + 1);

        assert_eq!(
            err.trim(),
            tsc_message.trim(),
            "output vs tsc in {}",
            spec.filename
        );

        if update {
            fs::write(
                format!("{}.stderr", spec.filename),
                err.to_string().as_bytes(),
            )
            .unwrap();
        } else {
            assert_eq!(
                err.to_string().trim(),
                spec.expected_stderr.trim(),
                "output vs expected in {}",
                spec.filename
            );
        }
    }
}
