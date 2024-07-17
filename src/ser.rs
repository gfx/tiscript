use serde::Serialize;

use crate::value::Value;

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
