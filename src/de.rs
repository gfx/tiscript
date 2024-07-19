use std::{fs::read_to_string, path::Path, str::FromStr};

use serde::Deserialize;

use crate::{
    util::eval,
    value::{de::from_value, Value},
};

/// Evaluate the given source code and deserialize it into a Rust deserializable object.
pub fn from_str<'de, T>(source: &str) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let value = Value::from_str(source)?;
    let object = from_value(value)?;
    Ok(object)
}

/// Evaluate the given source file and deserialize it into a Rust deserializable object.
pub fn from_file<'de, T>(source_file: &str) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    from_path(Path::new(source_file))
}

/// Evaluate the given source file and deserialize it into a Rust deserializable object.
pub fn from_path<'de, T>(source_file: &Path) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let source = read_to_string(source_file)?;
    let value = eval(&source, source_file)?;
    let object = from_value(value)?;
    Ok(object)
}
