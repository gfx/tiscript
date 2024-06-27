// The CLI reads the source code of Titys program from stdin or the first arg, evaluates it, and prints the result to stdout.

use std::{
    io::{BufRead, BufReader},
    process::exit,
    rc::Rc,
};

use titys::{
    compiler::Compiler,
    dprintln,
    file_io::{parse_program, read_program},
    is_debug, set_debug,
    type_checker::{type_check, TypeCheckContext},
    value::Value,
    vm::{debugger, Vm, YieldResult},
};

struct Params {
    pub check: bool, // -c: compile only
    pub ast: bool,   // -a: show AST
    pub source_file: String,
    pub source: String,
}

fn show_help(cmd: &str, exit_code: i32) {
    print!(
        r#"Usage: {cmd} [--check] [--show-ast] [--debug] [--eval <source>] [source.ts]
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

    let mut next_arg = args.next();
    while let Some(arg) = next_arg {
        match &arg as &str {
            "--check" => check = true,
            "--show-ast" => ast = true,
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
            _ => {
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
        ast,
        source_file: source_file.unwrap(),
        source: source.unwrap(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let params = parse_params();
    let mut buf = vec![];
    let stmts = parse_program(&params.source_file, &params.source)?;

    match type_check(&stmts, &mut TypeCheckContext::new()) {
        Ok(_) => {
            dprintln!("{}: Type-check Ok.", params.source_file)
        }
        Err(e) => {
            return Err(format!(
                "{}:{}:{}: {}",
                params.source_file,
                e.span.location_line(),
                e.span.get_utf8_column(),
                e
            )
            .into())
        }
    }

    let mut compiler = Compiler::new();
    compiler.compile(&stmts)?;
    compiler.write_funcs(&mut std::io::Cursor::new(&mut buf))?;

    let bytecode = Rc::new(read_program(&mut std::io::Cursor::new(&mut buf))?);

    if params.ast {
        println!("{stmts:?}");
        return Ok(());
    }

    if params.check {
        println!("{}: Compile Ok.", params.source_file);
        return Ok(());
    }

    let mut vm =Vm::new(bytecode, Box::new(()), is_debug());
    if let Err(e) = vm.init_fn("main", &[]) {
        eprintln!("init_fn error: {e:?}");
    }
    loop {
        match vm.interpret() {
            Ok(YieldResult::Finished(_)) => break,
            Ok(YieldResult::Suspend(value)) => {
                dprintln!("Execution suspended with a yielded value {value}");
                if value == Value::Str("break".to_string()) {
                    if debugger(&vm) {
                        break;
                    }
                }
            }
            Err(e) => {
                eprintln!("Runtime error: {e:?}");
                break;
            }
        }
    }

    Ok(())
}
