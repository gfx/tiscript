// The CLI reads the source code of TiScript program from stdin or the first arg, evaluates it, and prints the result to stdout in JSON.

use std::{
    io::{BufRead, BufReader, Write},
    path::Path,
    process::exit,
};

use tiscript::{
    compiler::Compiler,
    set_debug,
    type_checker::{type_check, TypeCheckContext},
    util::{eval, parse_program, EvalOptions},
};

struct Params {
    pub check: bool,
    pub show_ast: bool,
    pub disasm: bool,
    pub compact: bool,
    pub source_file: String,
    pub source: String,
}

fn show_help(cmd: &str, exit_code: i32) {
    println!(
        r#"Usage: {cmd} [--check] [--show-ast] [--disasm] [--compact|--pretty] [--debug] [--eval <source>] [source.ts]"#
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
    let mut show_ast = false;
    let mut disasm = false;
    let mut compact = false;
    let mut no_more_option = false;

    let mut next_arg = args.next();
    while let Some(arg) = next_arg {
        match &arg as &str {
            "--check" => check = true,
            "--show-ast" => show_ast = true,
            "--disasm" => disasm = true,
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
        show_ast,
        disasm,
        compact,
        source_file: source_file.unwrap(),
        source: source.unwrap(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let params: Params = parse_params();

    if params.show_ast {
        let stmts = parse_program(&params.source, Path::new(&params.source_file))?;
        println!("AST: {:#?}", stmts);
        return Ok(());
    } else if params.check {
        let stmts = parse_program(&params.source, Path::new(&params.source_file))?;
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
    } else if params.disasm {
        let stmts = parse_program(&params.source, Path::new(&params.source_file))?;
        let mut ctx = TypeCheckContext::new();
        match type_check(&stmts, &mut ctx) {
            Ok(_) => {}
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

        let mut compiler = Compiler::new();
        compiler.compile(&stmts)?;
        compiler.disasm(&mut std::io::stdout().lock())?;
        return Ok(());
    }
    let exports = eval(
        &params.source,
        &EvalOptions::new_from_path_str(&params.source_file),
    )?;

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
    use serde_json::json;

    fn eval_to_json(source: &str) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let exports = eval(
            source,
            &EvalOptions {
                source_file: Path::new("test.ts"),
                timeout: None,
                debug: true,
            },
        )?;
        let json = serde_json::ser::to_string(&exports)?;
        let json_object = serde_json::from_str(&json)?;
        Ok(json_object)
    }

    #[test]
    fn test_scalar_num_int() {
        let result = eval_to_json("export const x: number = 42;").unwrap();
        assert_eq!(result, json!({"x": 42}));
    }

    #[test]
    fn test_scalar_num_fract() {
        let result = eval_to_json("export const x: number = 3.14;").unwrap();
        assert_eq!(result, json!({"x": 3.14}));
    }

    #[test]
    fn test_scalar_dq_str_simple() {
        let result = eval_to_json("export const x: string = \"hello, world!\";").unwrap();
        assert_eq!(result, json!({"x": "hello, world!"}));
    }

    #[test]
    fn test_scalar_dq_str_empty() {
        let result = eval_to_json("export const x: string = \"hello, world!\";").unwrap();
        assert_eq!(result, json!({"x": "hello, world!"}));
    }

    #[test]
    fn test_scalar_dq_str_with_newline_escapes() {
        let result = eval_to_json("export const x: string = \"hello, world!\\n\";").unwrap();
        assert_eq!(result, json!({"x": "hello, world!\n"}));
    }

    #[test]
    fn test_scalar_dq_str_with_unicode_escapes() {
        let result = eval_to_json("export const x: string = \"hello, \\u{2126}!\";").unwrap();
        assert_eq!(result, json!({"x": "hello, Ω!"})); // U+2126: Ohm sign
    }

    #[test]
    fn test_scalar_sq_str_empty() {
        let result = eval_to_json("export const x: string = '';").unwrap();
        assert_eq!(result, json!({"x": ""}));
    }

    #[test]
    fn test_scalar_sq_str_with_escapes() {
        let result = eval_to_json("export const x: string = 'hello, world!\\n';").unwrap();
        assert_eq!(result, json!({"x": "hello, world!\n"}));
    }

    #[test]
    fn test_scalar_sq_str_with_unicode_escapes() {
        let result = eval_to_json("export const x: string = 'hello, \\u{2126}!';").unwrap();
        assert_eq!(result, json!({"x": "hello, Ω!"})); // U+2126: Ohm sign
    }

    #[test]
    fn test_multiple_exports() {
        // multiple exports
        let result = eval_to_json(
            r#"
            export const one: number = 1;
            export const two: number = 2;
            export const pi: number = 3.14;
            export const hello: string = "world";
        "#,
        )
        .unwrap();
        assert_eq!(
            result,
            json!({"one": 1, "two": 2, "pi": 3.14, "hello": "world"})
        );
    }

    #[test]
    fn test_number_add() {
        let result = eval_to_json(
            r#"
            const one: number = 1;
            const two: number = 2;
            export const three: number = one + two;
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({"three": 3}));
    }

    // BigInt is not serializable to JSON
    #[test]
    fn test_bigint_literals() {
        let result = eval_to_json(
            r#"
            export const zero = 0n;
            export const answer = 42n;
            export const minus = -42n;
            export const w_underscore = 1_000_000n;
            export const i32_max = 2147483647n;
            export const i64_max = 9223372036854775807n;
        "#,
        )
        .unwrap();
        assert_eq!(
            result,
            json!(
                {
                    "zero": "0",
                    "answer": "42",
                    "minus": "-42",
                    "w_underscore": "1000000",
                    "i32_max": "2147483647",
                    "i64_max": "9223372036854775807"
                }
            )
        );
    }

    #[test]
    fn test_bigint_add() {
        let result = eval_to_json(
            r#"
            export const a = 1n + 42n;
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({ "a": "43" }));
    }

    #[test]
    fn test_bigint_sub() {
        let result = eval_to_json(
            r#"
            export const a = 42n - 1n;
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({ "a": "41" }));
    }

    #[test]
    fn test_bigint_mul() {
        let result = eval_to_json(
            r#"
            export const a = 42n * 2n;
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({ "a": "84" }));
    }

    #[test]
    fn test_bigint_div() {
        let result = eval_to_json(
            r#"
            export const a = 42n / 3n;
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({ "a": "14" }));
    }

    // TODO: literal types are not yet supported so the error message does not match TypeScript compiler's.
    #[test]
    fn test_err_add_number_and_bigint() {
        let result = eval_to_json(
            r#"
            export const a = 1 + 42n;
        "#,
        )
        .unwrap_err();
        assert_eq!(
            result.to_string(),
            "test.ts:2:30: Operator '+' cannot be applied to types 'number' and 'bigint'."
        );
    }

    #[test]
    fn test_err_add_bigint_and_number() {
        let result = eval_to_json(
            r#"
            export const a = 1n + 42;
        "#,
        )
        .unwrap_err();
        assert_eq!(
            result.to_string(),
            "test.ts:2:30: Operator '+' cannot be applied to types 'bigint' and 'number'."
        );
    }

    #[test]
    fn test_array_empty() {
        let result = eval_to_json(
            r#"
            export const array = [];
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({ "array": [] }));
    }

    #[test]
    fn test_object_empty() {
        let result = eval_to_json(
            r#"
            export const object = {};
        "#,
        )
        .unwrap();
        assert_eq!(result, json!({ "object": {} }));
    }

    #[test]
    fn test_p() {
        let result = eval_to_json(
            r#"
            p({ x: 1, y: 2 });
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_var_failure() {
        let result = eval_to_json(
            r#"
            var foo = 42;
        "#,
        );

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "test.ts:2:17: Keyword 'var' is not supported. Use 'let' or 'const' instead."
        );
    }
}
