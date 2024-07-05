use std::{any::Any, collections::HashMap, io::Write, rc::Rc};

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

    funcs.insert("sqrt".to_string(), unary_fn(f64::sqrt));
    funcs.insert("sin".to_string(), unary_fn(f64::sin));
    funcs.insert("cos".to_string(), unary_fn(f64::cos));
    funcs.insert("tan".to_string(), unary_fn(f64::tan));
    funcs.insert("asin".to_string(), unary_fn(f64::asin));
    funcs.insert("acos".to_string(), unary_fn(f64::acos));
    funcs.insert("atan".to_string(), unary_fn(f64::atan));
    funcs.insert("atan2".to_string(), binary_fn(f64::atgan2));
    funcs.insert("pow".to_string(), binary_fn(f64::powf));
    funcs.insert("exp".to_string(), unary_fn(f64::exp));
    funcs.insert("log".to_string(), binary_fn(f64::log));
    funcs.insert("log10".to_string(), unary_fn(f64::log10));
    funcs.insert(
        "print".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Any,
            code: Box::new(print_fn),
        }),
    );
    funcs.insert(
        "dbg".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Any,
            code: Box::new(dbg_fn),
        }),
    );
    funcs.insert(
        "puts".to_string(),
        FnDecl::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Any,
            code: Box::new(puts_fn),
        }),
    );
    funcs.insert(
        "i64".to_string(),
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
        "f64".to_string(),
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
        "str".to_string(),
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

fn print_fn(_: &dyn Any, args: &[Value]) -> Value {
    for arg in args {
        print!("{} ", arg);
    }
    println!("");
    Value::Num(0.)
}

fn dbg_fn(_: &dyn Any, values: &[Value]) -> Value {
    println!("dbg: {:?}", values[0]);
    Value::Int(0)
}

fn puts_fn(_: &dyn Any, args: &[Value]) -> Value {
    for arg in args {
        print!("{}", arg);
    }
    Value::Num(0.)
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
