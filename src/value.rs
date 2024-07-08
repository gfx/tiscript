use std::{cell::RefCell, error::Error, fmt::Display, rc::Rc};

use indexmap::IndexMap;
use serde::Serialize;

use crate::vm::Vm;

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueKind {
    Undefined, // undefined
    Null,      // null
    Bool,      // boolean
    Num,       // number
    Int,       // bigint (actually i64)
    Str,       // string
    Array,     // array
    Object,    // object
    Coro,      // TODO: generator function
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    #[default]
    Undefined,
    Null,
    Bool(bool),
    Num(f64),
    Int(i64),
    Str(String),
    Array(Vec<Value>),
    Object(IndexMap<String, Value>),
    Coro(Rc<RefCell<Vm>>),
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
            Self::Array(_) => write!(f, "[...]"), // TODO
            Self::Object(_) => write!(f, "[object Object]"),
            Self::Coro(_) => write!(f, "<coroutine>"),
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ValueKind::*;
        write!(
            f,
            "{}",
            match self {
                Undefined => "undefined",
                Null => "null",
                Bool => "bool",
                Num => "number",
                Int => "bigint",
                Str => "string",
                Array => "array",
                Object => "object",
                Coro => "coroutine",
            }
        )
    }
}

impl Value {
    pub fn kind(&self) -> ValueKind {
        match self {
            Self::Undefined => ValueKind::Undefined,
            Self::Null => ValueKind::Null,
            Self::Bool(_) => ValueKind::Bool,
            Self::Num(_) => ValueKind::Num,
            Self::Int(_) => ValueKind::Int,
            Self::Str(_) => ValueKind::Str,
            Self::Array(_) => ValueKind::Array,
            Self::Object(_) => ValueKind::Object,
            Self::Coro(_) => ValueKind::Coro,
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Self::Undefined | Self::Null => false,
            Self::Num(value) => *value != 0.0 || value.is_nan(),
            Self::Int(value) => *value != 0,
            Self::Str(value) => !value.is_empty(),
            Self::Bool(value) => *value,
            _ => true,
        }
    }

    pub fn coerce_num(&self) -> Result<f64, String> {
        Ok(match self {
            Self::Num(value) => *value,
            Self::Int(_) => return Err("Cannot convert a BigInt value to a number".to_string()),
            Self::Str(value) => value.parse().unwrap_or(f64::NAN),
            _ => f64::NAN,
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

    pub fn must_be_str(&self) -> Result<&str, Box<dyn Error>> {
        match self {
            Value::Str(s) => Ok(s),
            _ => Err(format!("Expected string, found {:?}", self).into()),
        }
    }

    pub fn must_be_array(&self) -> Result<&Vec<Value>, Box<dyn Error>> {
        match self {
            Value::Array(ary) => Ok(ary),
            _ => Err(format!("Expected object, found {:?}", self).into()),
        }
    }

    pub fn must_be_map(&self) -> Result<&IndexMap<String, Value>, Box<dyn Error>> {
        match self {
            Value::Object(map) => Ok(map),
            _ => Err(format!("Expected object, found {:?}", self).into()),
        }
    }

    // ECMA-262 Number.isSafeInteger()
    pub fn number_is_safe_integer(n: f64) -> bool {
        n.fract() == 0.0 && n.is_finite() && n.abs() <= (2f64.powi(53) - 1.0)
    }

    pub fn is_safe_integer(&self) -> bool {
        match self {
            Self::Num(value) => Self::number_is_safe_integer(*value),
            _ => false,
        }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        use serde::ser::*;

        match self {
            Self::Undefined => serializer.serialize_unit(),
            Self::Null => serializer.serialize_none(),
            Self::Bool(value) => serializer.serialize_bool(*value),
            Self::Num(value) => {
                if Value::number_is_safe_integer(*value) {
                    serializer.serialize_i64(*value as i64)
                } else {
                    serializer.serialize_f64(*value)
                }
            }
            Self::Int(value) => serializer.serialize_i64(*value),
            Self::Str(value) => serializer.serialize_str(value),
            Self::Array(value) => value.serialize(serializer),
            Self::Object(value) => {
                let mut map = serializer.serialize_map(Some(value.len()))?;
                for (k, v) in value.iter() {
                    map.serialize_entry(k, v)?;
                }
                map.end()
            }
            Self::Coro(_) => Err(serde::ser::Error::custom("Cannot serialize coroutine")),
        }
    }
}
