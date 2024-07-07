use std::{collections::HashMap, error::Error};

use crate::{
    ast::{ExprEnum, Expression, Span, Statement, TypeDecl},
    bytecode::{standard_functions, FnDecl, NativeFn, UserFn},
    parser::{calc_offset, GetSpan},
};

pub struct TypeCheckContext<'src, 'ctx> {
    /// Variables table for type checking.
    vars: HashMap<&'src str, TypeDecl>,
    /// Function names are owned strings because it can be either from source or native.
    funcs: HashMap<String, FnDecl<'src>>,
    super_context: Option<&'ctx TypeCheckContext<'src, 'ctx>>,
}

impl<'src, 'ctx> TypeCheckContext<'src, 'ctx> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: standard_functions(),
            super_context: None,
        }
    }

    pub fn add_fn(&mut self, name: String, fn_decl: NativeFn<'src>) {
        self.funcs.insert(name, FnDecl::Native(fn_decl));
    }

    fn get_var(&self, name: &str) -> Option<TypeDecl> {
        if let Some(val) = self.vars.get(name) {
            Some(val.clone())
        } else {
            None
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDecl<'src>> {
        if let Some(val) = self.funcs.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }
}

#[derive(Debug)]
pub struct TypeCheckError<'src> {
    pub msg: String,
    pub span: Span<'src>,
}

impl<'src> std::fmt::Display for TypeCheckError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\nlocation: {}:{}: {}",
            self.msg,
            self.span.location_line(),
            self.span.get_utf8_column(),
            self.span.fragment()
        )
    }
}

impl<'src> Error for TypeCheckError<'src> {}

impl<'src> TypeCheckError<'src> {
    fn new(msg: String, span: Span<'src>) -> Self {
        Self { msg, span }
    }
}

fn tc_coerce_type<'src>(
    value: &TypeDecl,
    target: &TypeDecl,
    span: Span<'src>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    Ok(match (value, target) {
        (_, Any) => value.clone(),
        (Any, _) => target.clone(),
        (Undefined, Undefined) => Undefined,
        (Null, Null) => Null,
        (Bool, Bool) => Bool,
        (Num , Num) => Num,
        (Int, Int) => Int,
        (Str, Str) => Str,
        (Coro, Coro) => Coro,
        _ => {
            return Err(TypeCheckError::new(
                format!("Type '{value}' is not assignable to type '{target}'."),
                span,
            ))
        }
    })
}

fn tc_binary_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    binary_op_type(&lhst, &rhst).map_err(|_| {
        TypeCheckError::new(
            format!(
                "Operator '{}' cannot be applied to types '{}' and '{}'.",
                op, lhst, rhst,
            ),
            lhs.span,
        )
    })
}

fn binary_op_type(lhs: &TypeDecl, rhs: &TypeDecl) -> Result<TypeDecl, ()> {
    use TypeDecl::*;
    Ok(match (lhs, rhs) {
        (Any, _) => Any,
        (_, Any) => Any,
        (Bool, Bool) => Bool,
        (Int, Int) => Int,
        (Num | Int, Num | Int) => Num,
        (Str, Str) => Str,
        _ => return Err(()),
    })
}

fn tc_binary_cmp<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    Ok(match (&lhst, &rhst) {
        (Any, _) => Int,
        (_, Any) => Int,
        (Undefined | Null, _) => Int,
        (_, Undefined | Null) => Int,
        (Bool, Bool) => Int,
        (Num, Num) => Int,
        (Int, Int) => Int,
        (Str, Str) => Int,
        _ => {
            return Err(TypeCheckError::new(
                format!(
                    "Operation {op} between incompatible type: {:?} and {:?}",
                    lhst, rhst,
                ),
                lhs.span,
            ))
        }
    })
}

fn tc_expr<'src>(
    e: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use ExprEnum::*;
    Ok(match &e.expr {
        UndefinedLiteral => TypeDecl::Undefined,
        NullLiteral => TypeDecl::Null,
        BoolLiteral(_val) => TypeDecl::Bool,
        NumLiteral(_val) => TypeDecl::Num,
        BigIntLiteral(_val) => TypeDecl::Int,
        StrLiteral(_val) => TypeDecl::Str,
        Ident(str) => ctx.get_var(str).ok_or_else(|| {
            TypeCheckError::new(format!("Variable \"{}\" not found in scope", str), e.span)
        })?,
        FnInvoke(str, args) => {
            let args_ty = args
                .iter()
                .map(|v| Ok((tc_expr(v, ctx)?, v.span)))
                .collect::<Result<Vec<_>, _>>()?;
            let func = ctx.get_fn(**str).ok_or_else(|| {
                TypeCheckError::new(format!("function {} is not defined", str), *str)
            })?;
            let args_decl = func.args();
            for ((arg_ty, arg_span), decl) in args_ty.iter().zip(args_decl.iter()) {
                tc_coerce_type(&arg_ty, &decl.1, *arg_span)?;
            }
            func.ret_type()
        }
        Not(_ex) => {
            TypeDecl::Bool
        }
        Minus(ex) => {
            if let Err(_) = tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Int, ex.span) {
                tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Num, ex.span)?;
            }
            TypeDecl::Num
        }
        Plus(ex) => {
            let td = tc_expr(ex, ctx)?;
            if let Err(_) = tc_coerce_type(&td, &TypeDecl::Num, ex.span) {
                return Err(TypeCheckError::new(
                    format!("Operator '+' cannot be applied to type '{td}'."),
                    ex.span,
                ));
            }
            TypeDecl::Num
        }
        Add(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "+")?,
        Sub(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "-")?,
        Mul(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "*")?,
        Div(lhs, rhs) => tc_binary_op(&lhs, &rhs, ctx, "/")?,
        Lt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, "<")?,
        Gt(lhs, rhs) => tc_binary_cmp(&lhs, &rhs, ctx, ">")?,
        If(cond, true_branch, false_branch) => {
            tc_coerce_type(&tc_expr(cond, ctx)?, &TypeDecl::Int, cond.span)?;
            let true_type = type_check(true_branch, ctx)?;
            if let Some(false_branch) = false_branch {
                let false_type = type_check(false_branch, ctx)?;
                binary_op_type(&true_type, &false_type).map_err(|_| {
                    let true_span = true_branch.span();
                    let false_span = false_branch.span();
                    TypeCheckError::new(
                        format!(
                            "Conditional expression doesn't have the \
              compatible types in true and false branch: \
              {:?} and {:?}",
                            true_type, false_type
                        ),
                        calc_offset(true_span, false_span),
                    )
                })?
            } else {
                true_type
            }
        }
        Await(ex) => {
            let _res = tc_expr(ex, ctx)?;
            TypeDecl::Any
        }
    })
}

pub fn type_check<'src>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let mut res = TypeDecl::Any;
    for stmt in stmts {
        match stmt {
            Statement::VarDef { name, td, ex, .. } => {
                let mut init_type = tc_expr(ex, ctx)?;
                if let Some(td) = td {
                    init_type = tc_coerce_type(&init_type, td, ex.span)?;
                }
                ctx.vars.insert(**name, init_type);
            }
            Statement::VarAssign { name, ex, .. } => {
                let init_type = tc_expr(ex, ctx)?;
                let target = ctx.vars.get(**name).expect("Variable not found");
                tc_coerce_type(&init_type, target, ex.span)?;
            }
            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
                is_cofn,
            } => {
                // Function declaration needs to be added first to allow recursive calls
                ctx.funcs.insert(
                    name.to_string(),
                    FnDecl::User(UserFn::new(args.clone(), *ret_type, *is_cofn)),
                );
                let mut subctx = TypeCheckContext::push_stack(ctx);
                for (arg, ty) in args.iter() {
                    subctx.vars.insert(arg, *ty);
                }
                let last_stmt = type_check(stmts, &mut subctx)?;
                tc_coerce_type(&last_stmt, &ret_type, stmts.span())?;
            }
            Statement::Expression(e) => {
                res = tc_expr(&e, ctx)?;
            }
            Statement::Return(e) => {
                return tc_expr(e, ctx);
            }
            Statement::Yield(e) => {
                tc_expr(e, ctx)?;
                // TODO: check type with the return type, but don't escape from this function.
            }
            Statement::ExportDefault(e) => {
                res = tc_expr(e, ctx)?;
            }
            Statement::Export(stmts) => {
                res = type_check(stmts, ctx)?;
            }
        }
    }
    Ok(res)
}
