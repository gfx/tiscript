use std::{any::Any, collections::HashMap, io::Write, rc::Rc};

use indexmap::IndexMap;

use crate::{
    ast::{Span, TypeDecl},
    instructions::{Instruction, OpCode},
    value::Value,
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
            LoadLiteral => writeln!(
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

pub struct NativeFn<'src> {
    args: Vec<(&'src str, TypeDecl)>,
    ret_type: TypeDecl,
    pub(crate) code: Box<dyn Fn(&dyn Any, &[Value]) -> Value>,
}

impl<'src> NativeFn<'src> {
    pub fn new(
        args: Vec<(&'src str, TypeDecl)>,
        ret_type: TypeDecl,
        code: Box<dyn Fn(&dyn Any, &[Value]) -> Value>,
    ) -> Self {
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

type Functions<'src> = HashMap<String, FnDecl<'src>>;

pub(crate) fn standard_functions<'src>() -> Functions<'src> {
    let mut funcs = Functions::new();

    funcs.insert(
        "Array.from".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("args", TypeDecl::Any)],
            ret_type: TypeDecl::Array, // TODO: Array<T>
            code: Box::new(|_, args| Value::Array(args.into())),
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
    funcs.insert("Math.clz32".to_string(), unary_fn(|x| x.to_bits().leading_zeros() as f64));
    funcs.insert("Math.cos".to_string(), unary_fn(f64::cos));
    funcs.insert("Math.cosh".to_string(), unary_fn(f64::cosh));
    funcs.insert("Math.exp".to_string(), unary_fn(f64::exp));
    funcs.insert("Math.expm1".to_string(), unary_fn(f64::exp_m1));
    funcs.insert("Math.floor".to_string(), unary_fn(f64::floor));
    funcs.insert("Math.fround".to_string(), unary_fn(f64::round));
    funcs.insert("Math.hypot".to_string(), FnDecl::Native(NativeFn {
        args: vec![("args", TypeDecl::Any)],
        ret_type: TypeDecl::Num,
        code: Box::new(move |_, args| {
            let mut sum = 0.;
            for arg in args {
                sum += arg.coerce_num().unwrap();
            }
            Value::Num(sum)
        }),
    }));
    funcs.insert("Math.imul".to_string(), binary_fn(|lhs, rhs| (lhs as i32).wrapping_mul(rhs as i32) as f64));
    funcs.insert("Math.log".to_string(), unary_fn(f64::ln));
    funcs.insert("Math.log10".to_string(), unary_fn(f64::log10));
    funcs.insert("Math.log1p".to_string(), unary_fn(f64::ln_1p));
    funcs.insert("Math.log2".to_string(), unary_fn(f64::log2));
    funcs.insert("Math.max".to_string(), FnDecl::Native(NativeFn {
        args: vec![("args", TypeDecl::Any)],
        ret_type: TypeDecl::Num,
        code: Box::new(move |_, args| {
            let max = args
                .into_iter()
                .map(|arg| arg.coerce_num().unwrap())
                .fold(f64::NEG_INFINITY, f64::max);
            Value::Num(max)
        }),
    }));
    funcs.insert("Math.min".to_string(), FnDecl::Native(NativeFn {
        args: vec![("args", TypeDecl::Any)],
        ret_type: TypeDecl::Num,
        code: Box::new(move |_, args| {
            let min = args
                .into_iter()
                .map(|arg| arg.coerce_num().unwrap())
                .fold(f64::INFINITY, f64::min);
            Value::Num(min)
        }),
    }));
    funcs.insert("Math.pow".to_string(), binary_fn(f64::powf));
    funcs.insert("Math.random".to_string(), FnDecl::Native(NativeFn {
        args: vec![],
        ret_type: TypeDecl::Num,
        code: Box::new(|_, _| todo!("Math.random")),
    }));
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
            ret_type: TypeDecl::Any,
            code: Box::new(p_fn),
        }),
    );

    funcs.insert(
        "BigInt".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Int,
            code: Box::new(move |_, args| {
                Value::Int(
                    args.first()
                        .expect("function missing argument")
                        .coerce_int()
                        .unwrap_or(0),
                )
            }),
        }),
    );
    funcs.insert(
        "Number".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Num,
            code: Box::new(move |_, args| {
                Value::Num(
                    args.first()
                        .expect("function missing argument")
                        .coerce_num()
                        .unwrap_or(0.),
                )
            }),
        }),
    );
    funcs.insert(
        "String".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Str,
            code: Box::new(move |_, args| {
                Value::Str(
                    args.first()
                        .expect("function missing argument")
                        .coerce_str()
                        .unwrap_or("".to_string()),
                )
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
            Value::Num(f(args
                .into_iter()
                .next()
                .expect("function missing argument")
                .coerce_num()
                .unwrap()))
        }),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDecl<'a> {
    FnDecl::Native(NativeFn {
        args: vec![("lhs", TypeDecl::Num), ("rhs", TypeDecl::Num)],
        ret_type: TypeDecl::Num,
        code: Box::new(move |_, args| {
            let mut args = args.into_iter();
            let lhs = args
                .next()
                .expect("function missing the first argument")
                .coerce_num()
                .unwrap();
            let rhs = args
                .next()
                .expect("function missing the second argument")
                .coerce_num()
                .unwrap();
            Value::Num(f(lhs, rhs))
        }),
    })
}

fn object_from_entries_fn(_: &dyn Any, args: &[Value]) -> Value {
    let mut object = IndexMap::with_capacity(args.len());
    for arg in args {
        match arg {
            Value::Array(pair) => {
                let key = pair[0].clone();
                let value = pair[1].clone();
                object.insert(key.must_be_str().unwrap().to_string(), value);
            }
            _ => return Value::Undefined
        }
    }
    Value::Object(object)
}

fn p_fn(_: &dyn Any, values: &[Value]) -> Value {
    println!("{:?}", values[0]);
    Value::Int(0)
}

pub struct ByteCode {
    pub(crate) funcs: HashMap<String, FnDef>,
}

impl ByteCode {
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
        }
    }

    pub fn add_fn(&mut self, name: String, native_fn: NativeFn<'static>) {
        self.funcs.insert(name, FnDef::Native(native_fn));
    }
}
