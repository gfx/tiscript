use std::{path::Path, str::FromStr};

use serde::de::{DeserializeSeed, IntoDeserializer, SeqAccess, Unexpected, Visitor};

use crate::{util::eval, value::{Array, Map, Value}};

macro_rules! tri {
    ($e:expr $(,)?) => {
        match $e {
            core::result::Result::Ok(val) => val,
            core::result::Result::Err(err) => return core::result::Result::Err(err.into()),
        }
    };
}

#[derive(Debug)]
pub struct Error(String);

impl std::error::Error for Error {
    fn description(&self) -> &str {
        &self.0
    }
}
impl serde::de::Error for Error {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        Self(msg.to_string())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Box<dyn std::error::Error>> for Error {
    fn from(err: Box<dyn std::error::Error>) -> Self {
        Self(err.to_string())
    }
}

impl From<&'_ str> for Error {
    fn from(err: &str) -> Self {
        Self(err.to_string())
    }
}

impl From<String> for Error {
    fn from(err: String) -> Self {
        Self(err)
    }
}

impl Value {
    #[cold]
    #[allow(dead_code)]
    fn unexpected(&self) -> Unexpected {
        match self {
            Value::Null => Unexpected::Unit,
            Value::Bool(b) => Unexpected::Bool(*b),
            Value::Num(n) => Unexpected::Float(*n),
            Value::Int(n) => Unexpected::Signed(*n),
            Value::Str(s) => Unexpected::Str(s),
            Value::Array(_) => Unexpected::Seq,
            Value::Object(_) => Unexpected::Map,
            _ => Unexpected::Other("coroutine"),
        }
    }
}

struct SeqDeserializer<'a> {
    iter: std::slice::Iter<'a, Value>,
}

impl<'a> SeqDeserializer<'a> {
    fn new(vec: &'a Vec<Value>) -> Self {
        SeqDeserializer { iter: vec.iter() }
    }
}

impl<'a, 'de> SeqAccess<'de> for SeqDeserializer<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some(value) => seed.deserialize(value.clone()).map(Some), // TODO: clone
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

fn visit_array<'de, V>(array: Array, visitor: V) -> Result<V::Value, Error>
where
    V: Visitor<'de>,
{
    let len = array.len();
    let mut deserializer = SeqDeserializer::new(&array);
    let seq = tri!(visitor.visit_seq(&mut deserializer));
    let remaining = deserializer.iter.len();
    if remaining == 0 {
        Ok(seq)
    } else {
        Err(serde::de::Error::invalid_length(
            len,
            &"fewer elements in array",
        ))
    }
}

struct MapDeserializer<'a> {
    iter: indexmap::map::Iter<'a, String, Value>,
    value: Option<&'a Value>,
}

impl<'a> MapDeserializer<'a> {
    fn new(map: &'a Map) -> Self {
        MapDeserializer {
            iter: map.iter(),
            value: None,
        }
    }
}

impl<'a, 'de> serde::de::MapAccess<'de> for MapDeserializer<'a> {
    type Error = Error;

    fn next_key_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                let key_de = MapKeyDeserializer { key };
                seed.deserialize(key_de).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T>(&mut self, seed: T) -> Result<T::Value, Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.value.take() {
            Some(value) => seed.deserialize(value.clone()), // TODO: clone
            None => Err("value is missing".into()),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

struct MapKeyDeserializer<'a> {
    key: &'a String,
}

impl<'a, 'de> serde::Deserializer<'de> for MapKeyDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_string(self.key.clone())
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

fn visit_object<'de, V>(object: Map, visitor: V) -> Result<V::Value, Error>
where
    V: Visitor<'de>,
{
    let len = object.len();
    let mut deserializer = MapDeserializer::new(&object);
    let map = tri!(visitor.visit_map(&mut deserializer));
    let remaining = deserializer.iter.len();
    if remaining == 0 {
        Ok(map)
    } else {
        Err(serde::de::Error::invalid_length(
            len,
            &"fewer elements in map",
        ))
    }
}

impl<'de> IntoDeserializer<'de, Error> for Value {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self::Deserializer {
        self
    }
}

impl<'de> serde::de::Deserializer<'de> for Value {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            Value::Undefined => visitor.visit_unit(),
            Value::Null => visitor.visit_unit(),
            Value::Bool(value) => visitor.visit_bool(value),
            Value::Num(value) => visitor.visit_f64(value),
            Value::Int(value) => visitor.visit_i64(value),
            Value::Str(value) => visitor.visit_string(value),
            Value::Array(value) => visit_array(value, visitor),
            Value::Object(value) => visit_object(value, visitor),
            Value::Coro(_) => Err("Cannot deserialize a coroutine".into()),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.must_be_unit()?;
        visitor.visit_unit()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            Value::Undefined | Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'_ str,
        _variants: &'_ [&'_ str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(self.to_bool())
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i8(self.coerce_int()? as i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i16(self.coerce_int()? as i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i32(self.coerce_int()? as i32)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i64(self.coerce_int()?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u8(self.coerce_uint()? as u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u16(self.coerce_uint()? as u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(self.coerce_uint()? as u32)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u64(self.coerce_uint()? as u64)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f32(self.coerce_num()? as f32)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f64(self.coerce_num()?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let s = self.coerce_str()?;
        let mut chars = s.chars();
        let c = chars
            .next()
            .ok_or_else(|| Error("empty string".to_string()))?;
        if chars.next().is_some() {
            return Err(Error("string too long".to_string()));
        }
        visitor.visit_char(c)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_string(self.coerce_str()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_string(self.coerce_str()?)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let s = self.coerce_str()?;
        visitor.visit_bytes(s.as_bytes())
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let s = self.coerce_str()?;
        visitor.visit_byte_buf(s.into_bytes())
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let seq = SeqDeserializer::new(self.must_be_array()?);
        visitor.visit_seq(seq)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'_ str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let map = MapDeserializer::new(self.must_be_object()?);
        visitor.visit_map(map)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'_ str,
        _fields: &'_ [&'_ str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            Value::Array(v) => visit_array(v, visitor),
            Value::Object(v) => visit_object(v, visitor),
            _ => Err(format!("Expected array or object, found {:?}", self).into()),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        drop(self);
        visitor.visit_unit()
    }
}

fn from_value<'de, T>(value: Value) -> Result<T, Box<dyn std::error::Error>>
where
    T: serde::de::Deserialize<'de>,
{
    let value = tri!(serde::de::Deserialize::deserialize(value));
    Ok(value)
}

impl FromStr for Value {
    type Err = Box<dyn std::error::Error>;
    fn from_str(s: &str) -> Result<Value, Self::Err> {
        eval(s, Path::new("<eval>"))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn test_deserialize_scalars() {
        #[derive(serde::Deserialize, PartialEq, Debug)]
        struct Test {
            foo: String,
            bar: bool,
            baz: f64,
            bax: i64,
        }

        let value = Value::from_str(r#"
            export const foo = "hello";
            export const bar = true;
            export const baz = 3.14;
            export const bax = 42;
        "#).unwrap();
        let map: Test = from_value(value).unwrap();
        assert_eq!(map.foo, "hello");
        assert_eq!(map.bar, true);
        assert_eq!(map.baz, 3.14);
        assert_eq!(map.bax, 42);
    }

    #[test]
    fn test_deserialize_objects() {
        #[derive(serde::Deserialize, PartialEq, Debug)]
        struct Test {
            foo: Vec<String>,
            bar: HashMap<String, String>,
        }

        let value = Value::from_str(r#"
            export const foo = ["hello", "world"];
            export const bar = { "a": "b", "x": "y" };
        "#).unwrap();

        let map: Test = from_value(value).unwrap();
        assert_eq!(map.foo, vec!["hello", "world"]);
        assert_eq!(map.bar.get("a"), Some(&"b".to_string()));
        assert_eq!(map.bar.get("x"), Some(&"y".to_string()));
    }
}
