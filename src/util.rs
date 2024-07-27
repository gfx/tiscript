use std::{error::Error, path::Path, rc::Rc, time::Duration};

use crate::{
    ast::{Span, Statements},
    compiler::Compiler,
    is_debug,
    parser::statements_finish,
    type_checker::{type_check, TypeCheckContext},
    value::Value,
    vm::{Vm, YieldResult},
};

#[derive(Debug)]
pub struct EvalOptions<'a> {
    pub source_file: &'a Path,
    pub timeout: Option<Duration>,
}

impl<'a> EvalOptions<'a> {
    pub fn new(source_file: &'a Path) -> Self {
        Self {
            source_file,
            timeout: None,
        }
    }

    pub fn new_with_timeout(source_file: &'a Path, timeout: Duration) -> Self {
        Self {
            source_file,
            timeout: Some(timeout),
        }
    }

    pub fn new_from_path_str(source_file: &'a str) -> Self {
        Self::new(Path::new(source_file))
    }
}

pub fn parse_program<'src>(
    source: &'src str,
    source_file: &'src Path,
) -> Result<Statements<'src>, Box<dyn Error>> {
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

fn check_timeout(t0: std::time::Instant, options: &EvalOptions) -> Result<(), Box<dyn Error>> {
    if let Some(timeout) = options.timeout {
        let elapsed = t0.elapsed();
        if elapsed >= timeout {
            return Err(format!(
                "{}: timeout after {:?}",
                options.source_file.display(),
                elapsed
            )
            .into());
        }
    }
    Ok(())
}

pub fn eval<'a>(
    source: &'a str,
    options: &EvalOptions<'a>,
) -> Result<Value, Box<dyn std::error::Error>> {
    let t0 = std::time::Instant::now();

    let stmts = parse_program(source, options.source_file)?;

    check_timeout(t0, options)?;

    match type_check(&stmts, &mut TypeCheckContext::new()) {
        Ok(_) => {
            // nothing to do
        }
        Err(e) => {
            return Err(format!(
                "{}:{}:{}: {}",
                options.source_file.display(),
                e.span.location_line(),
                e.span.get_utf8_column(),
                e.msg
            )
            .into())
        }
    }

    check_timeout(t0, options)?;

    let mut compiler = Compiler::new();
    compiler.compile(&stmts)?;
    let bytecode = Rc::new(compiler.into_bytecode());

    check_timeout(t0, options)?;

    let mut vm = Vm::new(bytecode, Box::new(()), is_debug());
    if let Err(e) = vm.init_fn("main", &[]) {
        return Err(format!("init_fn error: {e:?}").into());
    }
    loop {
        match vm.interpret() {
            Ok(YieldResult::Finished(_)) => break,
            Ok(YieldResult::Suspend(_)) => {
                // nothing to do
            }
            Err(e) => {
                return Err(format!("Runtime error: {e:?}").into());
            }
        }
        check_timeout(t0, options)?;
    }

    Ok(vm.exports)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_timeout() {
        let mut source = String::new();

        // 10k times of statements
        for i in 0..1_0000 {
            let s = format!("export const a{i} = {i};\n");
            source.push_str(&s);
        }

        let options =
            EvalOptions::new_with_timeout(Path::new("test_timeout"), Duration::from_millis(1));
        let result = eval(&source, &options);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("test_timeout: timeout after"));
    }
}
