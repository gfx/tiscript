use std::{
    cell::RefCell,
    fmt::Display,
    io::{Read, Write},
    rc::Rc,
};

use crate::vm::Vm;

#[repr(u8)]
pub enum ValueKind {
    Undefined, // undefined
    Null, // null
    Bool, // boolean
    Num, // number
    Int, // bigint (actually i64)
    Str, // string
    Coro, // TODO: generator function
}

#[derive(Debug, Clone)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Num(f64),
    Int(i64),
    Str(String),
    Coro(Rc<RefCell<Vm>>),
}

impl Default for Value {
    fn default() -> Self {
        Self::Null
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Undefined, Undefined) => true,
            (Null, Null) => true,
            (Bool(lhs), Bool(rhs)) => lhs == rhs,
            (Num(lhs), Num(rhs)) => lhs == rhs,
            (Int(lhs), Int(rhs)) => lhs == rhs,
            (Str(lhs), Str(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined => write!(f, "undefined"),
            Self::Null => write!(f, "null"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::Num(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::Str(value) => write!(f, "{value}"),
            Self::Coro(_) => write!(f, "<Coroutine>"),
        }
    }
}

impl Value {
    fn kind(&self) -> ValueKind {
        match self {
            Self::Undefined => ValueKind::Undefined,
            Self::Null => ValueKind::Null,
            Self::Bool(_) => ValueKind::Bool,
            Self::Num(_) => ValueKind::Num,
            Self::Int(_) => ValueKind::Int,
            Self::Str(_) => ValueKind::Str,
            Self::Coro(_) => ValueKind::Coro,
        }
    }

    pub(crate) fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let kind = self.kind() as u8;
        writer.write_all(&[kind])?;
        match self {
            Self::Undefined => {
                // nothing to do
            }
            Self::Null => {
                // nothing to do
            }
            Self::Bool(value) => {
                writer.write_all(&[*value as u8])?;
            }
            Self::Num(value) => {
                writer.write_all(&value.to_le_bytes())?;
            }
            Self::Int(value) => {
                writer.write_all(&value.to_le_bytes())?;
            }
            Self::Str(value) => {
                serialize_str(value, writer)?;
            }
            Self::Coro(_) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Coroutine can't be serialized",
                ))
            }
        }
        Ok(())
    }

    #[allow(non_upper_case_globals)]
    pub(crate) fn deserialize(reader: &mut impl Read) -> std::io::Result<Self> {
        const Null: u8 = ValueKind::Null as u8;
        const Num: u8 = ValueKind::Num as u8;
        const Int: u8 = ValueKind::Int as u8;
        const Str: u8 = ValueKind::Str as u8;

        let mut kind_buf = [0u8; 1];
        reader.read_exact(&mut kind_buf)?;
        match kind_buf[0] {
            Null => Ok(Value::Null),
            Num => {
                let mut buf = [0u8; std::mem::size_of::<f64>()];
                reader.read_exact(&mut buf)?;
                Ok(Value::Num(f64::from_le_bytes(buf)))
            }
            Int => {
                let mut buf = [0u8; std::mem::size_of::<i64>()];
                reader.read_exact(&mut buf)?;
                Ok(Value::Int(i64::from_le_bytes(buf)))
            }
            Str => Ok(Value::Str(deserialize_str(reader)?)),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "ValueKind {} does not match to any known value",
                    kind_buf[0]
                ),
            )),
        }
    }

    pub fn coerce_num(&self) -> Result<f64, String> {
        Ok(match self {
            Self::Num(value) => *value,
            Self::Int(value) => *value as f64,
            _ => {
                return Err(format!(
                    "Coercion failed: {:?} cannot be coerced to number",
                    self
                ))
            }
        })
    }

    pub fn coerce_int(&self) -> Result<i64, String> {
        Ok(match self {
            Self::Num(value) => *value as i64,
            Self::Int(value) => *value,
            _ => {
                return Err(format!(
                    "Coercion failed: {:?} cannot be coerced to bigint",
                    self
                ))
            }
        })
    }

    pub fn coerce_str(&self) -> Result<String, String> {
        Ok(match self {
            Self::Num(value) => format!("{value}"),
            Self::Int(value) => format!("{value}"),
            Self::Str(value) => value.clone(),
            _ => {
                return Err(format!(
                    "Coercion failed: {:?} cannot be coerced to str",
                    self
                ))
            }
        })
    }

    pub fn must_be_str(&self) -> Result<&str, String> {
        match self {
            Value::Str(s) => Ok(s),
            _ => Err(format!("Expected string, found {:?}", self)),
        }
    }
}

pub fn serialize_size(sz: usize, writer: &mut impl Write) -> std::io::Result<()> {
    writer.write_all(&(sz as u32).to_le_bytes())
}

pub fn deserialize_size(reader: &mut impl Read) -> std::io::Result<usize> {
    let mut buf = [0u8; std::mem::size_of::<u32>()];
    reader.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf) as usize)
}

pub fn serialize_str(s: &str, writer: &mut impl Write) -> std::io::Result<()> {
    serialize_size(s.len(), writer)?;
    writer.write_all(s.as_bytes())?;
    Ok(())
}

pub fn deserialize_str(reader: &mut impl Read) -> std::io::Result<String> {
    let mut buf = vec![0u8; deserialize_size(reader)?];
    reader.read_exact(&mut buf)?;
    let s = String::from_utf8(buf).unwrap();
    Ok(s)
}
