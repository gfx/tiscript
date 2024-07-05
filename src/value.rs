use std::{
    cell::RefCell,
    fmt::Display,
    rc::Rc,
};

use indexmap::IndexMap;

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

#[derive(Debug, Clone)]
pub enum Value {
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

impl Default for Value {
    fn default() -> Self {
        Self::Undefined
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
