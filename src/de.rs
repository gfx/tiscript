use std::{fs::read_to_string, path::Path};

use serde::Deserialize;

use crate::{
    util::{eval, EvalOptions},
    value::de::from_value,
};

/// Evaluate the given source code and deserialize it into a Rust deserializable object.
pub fn from_str<'de, T>(source: &str) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let value = eval(source, &EvalOptions::new(Path::new("<eval>")))?;
    let object = from_value(value)?;
    Ok(object)
}

/// Evaluate the given source code and deserialize it into a Rust deserializable object.
pub fn from_str_with_timeout<'de, T>(
    source: &str,
    timeout: std::time::Duration,
) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let value = eval(
        source,
        &EvalOptions::new_with_timeout(Path::new("<eval>"), timeout),
    )?;
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
pub fn from_file_with_timeout<'de, T>(
    source_file: &str,
    timeout: std::time::Duration,
) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let source_file = Path::new(source_file);
    let source = read_to_string(source_file)?;
    let value = eval(
        &source,
        &EvalOptions::new_with_timeout(source_file, timeout),
    )?;
    let object = from_value(value)?;
    Ok(object)
}

/// Evaluate the given source file and deserialize it into a Rust deserializable object.
pub fn from_path<'de, T>(source_file: &Path) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let source = read_to_string(source_file)?;
    let value = eval(&source, &EvalOptions::new(source_file))?;
    let object = from_value(value)?;
    Ok(object)
}

/// Evaluate the given source file and deserialize it into a Rust deserializable object.
pub fn from_path_with_timeout<'de, T>(
    source_file: &Path,
    timeout: std::time::Duration,
) -> Result<T, Box<dyn std::error::Error>>
where
    T: Deserialize<'de>,
{
    let source = read_to_string(source_file)?;
    let value = eval(
        &source,
        &EvalOptions::new_with_timeout(source_file, timeout),
    )?;
    let object = from_value(value)?;
    Ok(object)
}
