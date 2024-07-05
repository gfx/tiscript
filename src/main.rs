// The CLI reads the source code of Titys program from stdin or the first arg, evaluates it, and prints the result to stdout.

use std::{
    io::{BufRead, BufReader, Write},
    process::exit,
};

use serde_json::json;

use titys::{
    set_debug,
    type_checker::{type_check, TypeCheckContext},
    util::eval,
    util::parse_program,
    value::Value,
};

struct Params {
    pub check: bool,
    pub show_ast: bool,
    pub compact: bool,
    pub source_file: String,
    pub source: String,
}

fn show_help(cmd: &str, exit_code: i32) {
    print!(
        r#"Usage: {cmd} [--check] [--show-ast] [--compact|--pretty] [--debug] [--eval <source>] [source.ts]
"#
    );
    exit(exit_code);
}

fn parse_params() -> Params {
    let mut is_eval = false;
    let mut args = std::env::args();

    let cmd = args.next().unwrap();
    let mut source_file: Option<String> = None;
    let mut source: Option<String> = None;
    let mut check = false;
    let mut ast = false;
    let mut compact = false;
    let mut no_more_option = false;

    let mut next_arg = args.next();
    while let Some(arg) = next_arg {
        match &arg as &str {
            "--check" => check = true,
            "--show-ast" => ast = true,
            "--compact" => {
                compact = true;
            }
            "--pretty" => {
                compact = false;
            }
            "--debug" => {
                set_debug(true);
            }
            "--eval" => {
                is_eval = true;
                source = args.next();
                if source.is_none() {
                    println!("No source code is specified");
                    exit(1);
                }
            }
            "--help" => show_help(&cmd, 0),
            "--" => no_more_option = true,
            _ => {
                if !no_more_option && arg.starts_with('-') {
                    println!("Unknown option: {arg}");
                    show_help(&cmd, 1);
                }

                if source_file.is_none() && !is_eval {
                    source_file = Some(arg);
                } else {
                    println!("More than one file names and/or --eval are specified");
                    show_help(&cmd, 1);
                }
            }
        }
        next_arg = args.next();
    }

    // (1): source_file is none (stdin), source is none
    // (2): source_file is a file, source is none
    // (3): source_file is none, source is a string (--eval)
    if source.is_none() {
        // either (1) or (2)
        let reader: Box<dyn BufRead> = if let Some(ref path) = source_file {
            Box::new(BufReader::new(std::fs::File::open(path).unwrap()))
        } else {
            Box::new(BufReader::new(std::io::stdin()))
        };
        if source_file.is_none() {
            source_file = Some("<stdin>".to_string())
        };
        source = Some(std::io::read_to_string(reader).unwrap());
    } else {
        // (3)
        source_file = Some("<eval>".to_string());
        assert!(source.is_some());
    }

    Params {
        check,
        show_ast: ast,
        compact,
        source_file: source_file.unwrap(),
        source: source.unwrap(),
    }
}

fn eval_to_json(params: &Params) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
    let exports = eval(&params.source_file, &params.source)?;
    let mut json = serde_json::Map::with_capacity(exports.len());
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

    Ok(serde_json::Value::Object(json))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let params: Params = parse_params();

    if params.show_ast {
        let stmts = parse_program(&params.source_file, &params.source)?;
        println!("AST: {:#?}", stmts);
        return Ok(());
    } else if params.check {
        let stmts = parse_program(&params.source_file, &params.source)?;
        let mut ctx = TypeCheckContext::new();
        match type_check(&stmts, &mut ctx) {
            Ok(_) => {
                println!("Type checking succeeded.");
            }
            Err(e) => {
                println!(
                    "{}:{}:{}: {}",
                    params.source_file,
                    e.span.location_line(),
                    e.span.get_utf8_column(),
                    e.msg
                );
                exit(1);
            }
        }
        return Ok(());
    }
    let exports = eval_to_json(&params)?;

    let mut stdout = std::io::stdout().lock();
    if params.compact {
        serde_json::to_writer(&mut stdout, &exports)?;
    } else {
        serde_json::to_writer_pretty(&mut stdout, &exports)?;
    }
    stdout.write_all(b"\n")?;

    Ok(())
}

// test eval_to_json()
#[cfg(test)]
mod tests {
    use super::*;
    use ctor::ctor;

    #[ctor]
    fn init() {
        set_debug(true);
    }

    fn simple_eval(source: &str) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let params: Params = Params {
            check: false,
            show_ast: false,
            compact: false,
            source_file: "test.ts".to_string(),
            source: source.to_string(),
        };
        eval_to_json(&params)
    }

    #[test]
    fn test_scalar_num_int() {
        let result = simple_eval("export let x: number = 42;").unwrap();
        assert_eq!(result, json!({"x": 42}));
    }

    #[test]
    fn test_scalar_num_fract() {
        let result = simple_eval("export let x: number = 3.14;").unwrap();
        assert_eq!(result, json!({"x": 3.14}));
    }

    #[test]
    fn test_scalar_dq_str_simple() {
        let result = simple_eval("export let x: string = \"hello, world!\";").unwrap();
        assert_eq!(result, json!({"x": "hello, world!"}));
    }

    #[test]
    fn test_scalar_dq_str_empty() {
        let result = simple_eval("export let x: string = \"hello, world!\";").unwrap();
        assert_eq!(result, json!({"x": "hello, world!"}));
    }

    #[test]
    fn test_scalar_dq_str_with_newline_escapes() {
        let result = simple_eval("export let x: string = \"hello, world!\\n\";").unwrap();
        assert_eq!(result, json!({"x": "hello, world!\n"}));
    }

    #[test]
    fn test_scalar_dq_str_with_unicode_escapes() {
        let result = simple_eval("export let x: string = \"hello, \\u{2126}!\";").unwrap();
        assert_eq!(result, json!({"x": "hello, Ω!"})); // U+2126: Ohm sign
    }

    #[test]
    fn test_scalar_sq_str_empty() {
        let result = simple_eval("export let x: string = '';").unwrap();
        assert_eq!(result, json!({"x": ""}));
    }

    #[test]
    fn test_scalar_sq_str_with_escapes() {
        let result = simple_eval("export let x: string = 'hello, world!\\n';").unwrap();
        assert_eq!(result, json!({"x": "hello, world!\n"}));
    }

    #[test]
    fn test_scalar_sq_str_with_unicode_escapes() {
        let result = simple_eval("export let x: string = 'hello, \\u{2126}!';").unwrap();
        assert_eq!(result, json!({"x": "hello, Ω!"})); // U+2126: Ohm sign
    }

    #[test]
    fn test_multiple_exports() {
        // multiple exports
        let result = simple_eval(
            r#"
            export let one: number = 1;
            export let two: number = 2;
            export let pi: number = 3.14;
            export let hello: string = "world";
        "#,
        )
        .unwrap();
        assert_eq!(
            result,
            json!({"one": 1, "two": 2, "pi": 3.14, "hello": "world"})
        );
    }

    #[test]
    fn test_adds() {
        // multiple exports
        let result = simple_eval(
            r#"
            let one: number = 1;
            let two: number = 2;
            export let three: number = one + two;
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({"three": 3}));
    }
}
