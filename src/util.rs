use std::{error::Error, io::Read, rc::Rc};

use indexmap::IndexMap;

use crate::{
    ast::{Span, Statements},
    bytecode::ByteCode,
    compiler::Compiler,
    is_debug,
    parser::statements_finish,
    type_checker::{type_check, TypeCheckContext},
    value::Value,
    vm::{Vm, YieldResult},
};

pub struct Params {
    pub check: bool,
    pub show_ast: bool,
    pub compact: bool,
    pub source_file: String,
    pub source: String,
}

pub fn parse_program<'src>(
    source_file: &str,
    source: &'src str,
) -> Result<Statements<'src>, Box<dyn Error>> {
    statements_finish(Span::new(source)).map_err(|e| {
        format!(
            "{}:{}:{}: {}",
            source_file,
            e.input.location_line(),
            e.input.get_utf8_column(),
            e
        )
        .into()
    })
}

pub fn read_program(reader: &mut impl Read) -> std::io::Result<Rc<ByteCode>> {
    let mut bytecode = ByteCode::new();
    bytecode.read_funcs(reader)?;
    Ok(Rc::new(bytecode))
}

pub fn eval<'a>(
    source_file: &'a str,
    source: &'a str,
) -> Result<IndexMap<String, Value>, Box<dyn std::error::Error>> {
    let stmts = parse_program(source_file, source)?;
    let mut buf = vec![];

    match type_check(&stmts, &mut TypeCheckContext::new()) {
        Ok(_) => {
            // nothing to do
        }
        Err(e) => {
            return Err(format!(
                "{}:{}:{}: {}",
                source_file,
                e.span.location_line(),
                e.span.get_utf8_column(),
                e.msg
            )
            .into())
        }
    }

    let mut compiler = Compiler::new();
    compiler.compile(&stmts)?;

    compiler.write_funcs(&mut std::io::Cursor::new(&mut buf))?;
    let bytecode = read_program(&mut std::io::Cursor::new(&mut buf))?;

    let mut vm = Vm::new(bytecode, Box::new(()), is_debug());
    if let Err(e) = vm.init_fn("main", &[]) {
        eprintln!("init_fn error: {e:?}");
    }
    loop {
        match vm.interpret() {
            Ok(YieldResult::Finished(_)) => break,
            Ok(YieldResult::Suspend(_)) => {
                // nothing to do
            }
            Err(e) => {
                eprintln!("Runtime error: {e:?}");
                break;
            }
        }
    }

    Ok(vm.exports)
}
