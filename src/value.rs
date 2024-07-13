use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc};

use indexmap::IndexMap;

use crate::vm::Vm;

pub type Map = IndexMap<String, Value>;
pub type Array = Vec<Value>;

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
    Array(Array),
    Object(Map),
    Coro(Rc<RefCell<Vm>>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Undefined, Value::Undefined) => true,
            (Value::Null, Value::Null) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Undefined
    }
}

impl<T> From<Option<T>> for Value
where
    T: Into<Value>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self::Null,
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Num(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Str(value)
    }
}

impl From<&'_ str> for Value {
    fn from(value: &str) -> Self {
        Self::Str(value.to_string())
    }
}

impl From<HashMap<String, Value>> for Value {
    fn from(f: HashMap<String, Value>) -> Self {
        Value::Object(Map::from_iter(f.into_iter()))
    }
}

impl From<Map> for Value {
    fn from(f: Map) -> Self {
        Value::Object(f)
    }
}

impl From<Array> for Value {
    fn from(f: Array) -> Self {
        Value::Array(f)
    }
}

impl From<&[Value]> for Value {
    fn from(f: &[Value]) -> Self {
        Value::Array(f.into())
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

    pub fn must_be_array(&self) -> Result<&Array, Box<dyn Error>> {
        match self {
            Value::Array(ary) => Ok(ary),
            _ => Err(format!("Expected object, found {:?}", self).into()),
        }
    }

    pub fn must_be_object(&self) -> Result<&Map, Box<dyn Error>> {
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

