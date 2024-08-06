use std::{any::Any, collections::HashMap, error::Error, io::Write, rc::Rc};

use crate::{
    ast::{Span, TypeDecl},
    instructions::{Instruction, OpCode},
    value::{Array, Map, Value},
};

pub struct FnByteCode {
    #[allow(dead_code)]
    pub(crate) args: Vec<String>,
    pub(crate) literals: Vec<Value>,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) cofn: bool,
}

impl FnByteCode {
    pub(crate) fn new(
        args: Vec<String>,
        literals: Vec<Value>,
        instructions: Vec<Instruction>,
        cofn: bool,
    ) -> Self {
        Self {
            args,
            literals,
            instructions,
            cofn,
        }
    }

    pub(crate) fn disasm(&self, writer: &mut impl Write) -> std::io::Result<()> {
        disasm_common(&self.literals, &self.instructions, writer)
    }
}

fn disasm_common(
    literals: &[Value],
    instructions: &[Instruction],
    writer: &mut impl Write,
) -> std::io::Result<()> {
    use OpCode::*;
    writeln!(writer, "  Literals [{}]", literals.len())?;
    for (i, con) in literals.iter().enumerate() {
        writeln!(writer, "    [{i}] {}", *con)?;
    }

    writeln!(writer, "  Instructions [{}]", instructions.len())?;
    for (i, inst) in instructions.iter().enumerate() {
        match inst.op {
            LoadLit => writeln!(
                writer,
                "    [{i}] {:?} {} ({:?})",
                inst.op, inst.arg0, literals[inst.arg0 as usize]
            )?,
            Copy | Dup | Call | Jmp | Jf | Pop | Store => {
                writeln!(writer, "    [{i}] {:?} {}", inst.op, inst.arg0)?
            }
            _ => writeln!(writer, "    [{i}] {:?}", inst.op)?,
        }
    }
    Ok(())
}
pub(crate) enum FnDecl<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}

impl<'src> FnDecl<'src> {
    pub fn args(&self) -> Vec<(&'src str, TypeDecl)> {
        match self {
            Self::User(user) => user
                .args
                .iter()
                .map(|(name, ty)| (*name.fragment(), *ty))
                .collect(),
            Self::Native(code) => code.args.clone(),
        }
    }

    pub fn ret_type(&self) -> TypeDecl {
        match self {
            Self::User(user) => {
                if user.cofn {
                    TypeDecl::Coro
                } else {
                    user.ret_type
                }
            }
            Self::Native(native) => native.ret_type,
        }
    }
}

pub struct UserFn<'src> {
    args: Vec<(Span<'src>, TypeDecl)>,
    ret_type: TypeDecl,
    cofn: bool,
}

impl<'src> UserFn<'src> {
    pub fn new(args: Vec<(Span<'src>, TypeDecl)>, ret_type: TypeDecl, cofn: bool) -> Self {
        Self {
            args,
            ret_type,
            cofn,
        }
    }
}

pub type NativeFnCode = Box<dyn Fn(&dyn Any, &[Value]) -> Result<Value, Box<dyn Error>>>;

pub struct NativeFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    pub(crate) code: NativeFnCode,
}

impl<'src> NativeFn<'src> {
    pub fn new(args: Vec<(&'src str, TypeDecl)>, ret_type: TypeDecl, code: NativeFnCode) -> Self {
        Self {
            args,
            ret_type,
            code,
        }
    }
}

pub enum FnDef {
    User(Rc<FnByteCode>),
    Native(NativeFn<'static>),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub td: TypeDecl,
    pub is_const: bool,
}

pub(crate) fn global_variables() -> HashMap<&'static str, VarDecl> {
    let vars = HashMap::new();
    // TODO
    vars
}

pub(crate) fn global_type_variables() -> HashMap<&'static str, TypeDecl> {
    let vars = HashMap::new();
    // TODO
    vars
}

pub(crate) fn global_functions<'src>() -> HashMap<String, FnDecl<'src>> {
    let mut funcs: HashMap<String, FnDecl> = HashMap::new();

    funcs.insert(
        "Array.of".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("...args", TypeDecl::Any)],
            ret_type: TypeDecl::Array, // TODO: ...args: Array<T>
            code: Box::new(|_, args| Ok(Value::Array(args.into()))),
        }),
    );
    funcs.insert(
        "Array.spread%".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![
                ("subarrays", TypeDecl::Array),
                ("spreadings", TypeDecl::Array),
            ], // TODO: subarrays: Array<Array<T>>, spreadings: Array<Array<T>>
            ret_type: TypeDecl::Array, // TODO: Array<T>
            code: Box::new(array_spread_fn),
        }),
    );
    funcs.insert(
        "Object.fromEntries".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("args", TypeDecl::Any)],
            ret_type: TypeDecl::Object, // TODO: Object<K, V>
            code: Box::new(object_from_entries_fn),
        }),
    );
    funcs.insert(
        "Object.assign".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("...args", TypeDecl::Object)],
            ret_type: TypeDecl::Object,
            code: Box::new(object_assign_fn),
        }),
    );
    // cf. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math
    funcs.insert("Math.abs".to_string(), unary_fn(f64::abs));
    funcs.insert("Math.acos".to_string(), unary_fn(f64::acos));
    funcs.insert("Math.acosh".to_string(), unary_fn(f64::acosh));
    funcs.insert("Math.asin".to_string(), unary_fn(f64::asin));
    funcs.insert("Math.asinh".to_string(), unary_fn(f64::asinh));
    funcs.insert("Math.atan".to_string(), unary_fn(f64::atan));
    funcs.insert("Math.atan2".to_string(), binary_fn(f64::atan2));
    funcs.insert("Math.atanh".to_string(), unary_fn(f64::atanh));
    funcs.insert("Math.cbrt".to_string(), unary_fn(f64::cbrt));
    funcs.insert("Math.ceil".to_string(), unary_fn(f64::ceil));
    funcs.insert(
        "Math.clz32".to_string(),
        unary_fn(|x| x.to_bits().leading_zeros() as f64),
    );
    funcs.insert("Math.cos".to_string(), unary_fn(f64::cos));
    funcs.insert("Math.cosh".to_string(), unary_fn(f64::cosh));
    funcs.insert("Math.exp".to_string(), unary_fn(f64::exp));
    funcs.insert("Math.expm1".to_string(), unary_fn(f64::exp_m1));
    funcs.insert("Math.floor".to_string(), unary_fn(f64::floor));
    funcs.insert("Math.fround".to_string(), unary_fn(f64::round));
    funcs.insert(
        "Math.hypot".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("args", TypeDecl::Any)],
            ret_type: TypeDecl::Num,
            code: Box::new(move |_, args| {
                let mut sum = 0.;
                for arg in args {
                    sum += arg.coerce_num().unwrap();
                }
                Ok(Value::Num(sum))
            }),
        }),
    );
    funcs.insert(
        "Math.imul".to_string(),
        binary_fn(|lhs, rhs| (lhs as i32).wrapping_mul(rhs as i32) as f64),
    );
    funcs.insert("Math.log".to_string(), unary_fn(f64::ln));
    funcs.insert("Math.log10".to_string(), unary_fn(f64::log10));
    funcs.insert("Math.log1p".to_string(), unary_fn(f64::ln_1p));
    funcs.insert("Math.log2".to_string(), unary_fn(f64::log2));
    funcs.insert(
        "Math.max".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("args", TypeDecl::Any)],
            ret_type: TypeDecl::Num,
            code: Box::new(move |_, args| {
                let max = args
                    .iter()
                    .map(|arg| arg.coerce_num().unwrap())
                    .fold(f64::NEG_INFINITY, f64::max);
                Ok(Value::Num(max))
            }),
        }),
    );
    funcs.insert(
        "Math.min".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("args", TypeDecl::Any)],
            ret_type: TypeDecl::Num,
            code: Box::new(move |_, args| {
                let min = args
                    .iter()
                    .map(|arg| arg.coerce_num().unwrap())
                    .fold(f64::INFINITY, f64::min);
                Ok(Value::Num(min))
            }),
        }),
    );
    funcs.insert("Math.pow".to_string(), binary_fn(f64::powf));
    funcs.insert(
        "Math.random".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![],
            ret_type: TypeDecl::Num,
            code: Box::new(|_, _| todo!("Math.random")),
        }),
    );
    funcs.insert("Math.round".to_string(), unary_fn(f64::round));
    funcs.insert("Math.sign".to_string(), unary_fn(f64::signum));
    funcs.insert("Math.sin".to_string(), unary_fn(f64::sin));
    funcs.insert("Math.sinh".to_string(), unary_fn(f64::sinh));
    funcs.insert("Math.sqrt".to_string(), unary_fn(f64::sqrt));
    funcs.insert("Math.tan".to_string(), unary_fn(f64::tan));
    funcs.insert("Math.tanh".to_string(), unary_fn(f64::tanh));
    funcs.insert("Math.trunc".to_string(), unary_fn(f64::trunc));

    funcs.insert(
        "p".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Undefined,
            code: Box::new(p_fn),
        }),
    );

    funcs.insert(
        "BigInt".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Int,
            code: Box::new(move |_, args| {
                Ok(Value::Int(
                    args.first()
                        .expect("function missing argument")
                        .coerce_int()
                        .unwrap_or(0),
                ))
            }),
        }),
    );
    funcs.insert(
        "Number".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Num,
            code: Box::new(move |_, args| {
                Ok(Value::Num(
                    args.first()
                        .expect("function missing argument")
                        .coerce_num()
                        .unwrap_or(f64::NAN),
                ))
            }),
        }),
    );
    funcs.insert(
        "String".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Str,
            code: Box::new(move |_, args| {
                Ok(Value::Str(
                    args.first()
                        .expect("function missing argument")
                        .coerce_str()
                        .unwrap_or("".to_string()),
                ))
            }),
        }),
    );
    funcs
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDecl<'a> {
    FnDecl::Native(NativeFn {
        args: vec![("lhs", TypeDecl::Num), ("rhs", TypeDecl::Num)],
        ret_type: TypeDecl::Num,
        code: Box::new(move |_, args| {
            Ok(Value::Num(f(args
                .iter()
                .next()
                .expect("function missing argument")
                .coerce_num()?)))
        }),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDecl<'a> {
    FnDecl::Native(NativeFn {
        args: vec![("lhs", TypeDecl::Num), ("rhs", TypeDecl::Num)],
        ret_type: TypeDecl::Num,
        code: Box::new(move |_, args| {
            let mut args = args.iter();
            let lhs = args
                .next()
                .expect("function missing the first argument")
                .coerce_num()?;
            let rhs = args
                .next()
                .expect("function missing the second argument")
                .coerce_num()?;
            Ok(Value::Num(f(lhs, rhs)))
        }),
    })
}

// [1, 2, 3, ...[4, 5, 6], 7, 8, 9, ...[10, 11, 12]]
// is transformed into (just like in template strings):
// Array.spread%([[1, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]])
fn array_spread_fn(_: &dyn Any, args: &[Value]) -> Result<Value, Box<dyn Error>> {
    let mut result: Array = Default::default();

    let mut args = args.iter();
    let subarrays = args
        .next()
        .expect("function missing the first argument")
        .must_be_array()?;
    let spreadings = args
        .next()
        .expect("function missing the second argument")
        .must_be_array()?;

    let mut i = 0;
    while i < subarrays.len() {
        let subarray = subarrays[i].must_be_array()?;
        for value in subarray {
            result.push(value.clone());
        }

        if i < spreadings.len() {
            let spreading = spreadings[i].must_be_array()?;
            for value in spreading {
                result.push(value.clone());
            }
        }
        i += 1;
    }

    Ok(Value::Array(result))
}

fn object_from_entries_fn(_: &dyn Any, args: &[Value]) -> Result<Value, Box<dyn Error>> {
    let entries = args
        .iter()
        .next()
        .expect("undefined is not iterable")
        .must_be_array()?;
    let mut object = Map::with_capacity(entries.len());
    for arg in entries {
        match arg {
            Value::Array(pair) => {
                let key = pair[0].clone();
                let value = pair[1].clone();
                object.insert(key.to_string(), value);
            }
            _ => return Ok(Value::Undefined),
        }
    }
    Ok(Value::Object(object))
}

fn object_assign_fn(_: &dyn Any, args: &[Value]) -> Result<Value, Box<dyn Error>> {
    let mut args = args.iter();
    let mut object = args
        .next()
        .expect("Cannot convert undefined or null to object")
        .must_be_object()?
        .clone();
    for arg in args {
        let arg = arg.must_be_object()?;
        for (key, value) in arg.iter() {
            object.insert(key.clone(), value.clone());
        }
    }
    Ok(Value::Object(object.clone()))
}

fn p_fn(_: &dyn Any, values: &[Value]) -> Result<Value, Box<dyn Error>> {
    println!("{:?}", values.iter().next().or(Some(&Value::Undefined)));
    Ok(Value::Undefined)
}

#[derive(Default)]
pub struct ByteCode {
    pub(crate) funcs: HashMap<String, FnDef>,
}

impl ByteCode {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_fn(&mut self, name: String, native_fn: NativeFn<'static>) {
        self.funcs.insert(name, FnDef::Native(native_fn));
    }
}
