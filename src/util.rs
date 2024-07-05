use std::{error::Error, path::Path, rc::Rc};

use indexmap::IndexMap;

use crate::{
    ast::{Span, Statements},
    compiler::Compiler,
    is_debug,
    parser::statements_finish,
    type_checker::{type_check, TypeCheckContext},
    value::Value,
    vm::{Vm, YieldResult},
};

pub fn parse_program<'a>(
    source: &'a str,
    source_file: &'a Path,
) -> Result<Statements<'a>, Box<dyn Error>> {
    statements_finish(Span::new(source)).map_err(|e| {
        format!(
            "{}:{}:{}: {}",
            source_file.display(),
            e.input.location_line(),
            e.input.get_utf8_column(),
            e
        )
        .into()
    })
}

pub fn eval<'a>(
    source: &'a str,
    source_file: &'a Path,
) -> Result<IndexMap<String, Value>, Box<dyn std::error::Error>> {
    let stmts = parse_program(source, source_file)?;

    match type_check(&stmts, &mut TypeCheckContext::new()) {
        Ok(_) => {
            // nothing to do
        }
        Err(e) => {
            return Err(format!(
                "{}:{}:{}: {}",
                source_file.display(),
                e.span.location_line(),
                e.span.get_utf8_column(),
                e.msg
            )
            .into())
        }
    }

    let mut compiler = Compiler::new();
    compiler.compile(&stmts)?;
    let bytecode = Rc::new(compiler.into_bytecode());

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
