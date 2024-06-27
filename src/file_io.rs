use std::{error::Error, io::Read};

use crate::{
    ast::{Span, Statements},
    bytecode::ByteCode,
    parser::statements_finish,
};

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

pub fn read_program(reader: &mut impl Read) -> std::io::Result<ByteCode> {
    let mut bytecode = ByteCode::new();
    bytecode.read_funcs(reader)?;
    Ok(bytecode)
}
