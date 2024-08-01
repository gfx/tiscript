use std::{collections::HashMap, error::Error};

use crate::{
    ast::{ExprEnum, Expression, Span, Statement, TypeDecl},
    bytecode::{standard_functions, FnDecl, NativeFn, UserFn},
    parser::GetSpan,
};

pub struct TypeCheckContext<'src, 'ctx> {
    /// Variables table for type checking.
    vars: HashMap<&'src str, VarDecl>,
    /// Function names are owned strings because it can be either from source or native.
    funcs: HashMap<String, FnDecl<'src>>,
    super_context: Option<&'ctx TypeCheckContext<'src, 'ctx>>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub td: TypeDecl,
    pub is_const: bool,
}

impl Default for TypeCheckContext<'_, '_> {
    fn default() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: standard_functions(),
            super_context: None,
        }
    }
}

impl<'src, 'ctx> TypeCheckContext<'src, 'ctx> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_fn(&mut self, name: String, fn_decl: NativeFn<'src>) {
        self.funcs.insert(name, FnDecl::Native(fn_decl));
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
        (_, Any) => *value,
        (Any, _) => *target,
        (Undefined, Undefined) => Undefined,
        (Null, Null) => Null,
        (Bool, Bool) => Bool,
        (Num, Num) => Num,
        (Int, Int) => Int,
        (Str, Str) => Str,
        (Array, Array) => Array,
        (Object, Object) => Object,
        (Coro, Coro) => Coro,
        _ => {
            return Err(TypeCheckError::new(
                format!("Type '{value}' is not assignable to type '{target}'."),
                span,
            ))
        }
    })
}

fn tc_bin_add_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    match (&lhst, &rhst) {
        (TypeDecl::Str, _) => Ok(TypeDecl::Str),
        (_, TypeDecl::Str) => Ok(TypeDecl::Str),
        _ => tc_bin_arithmetic_op(lhs, rhs, ctx, "+"),
    }
}

fn tc_bin_arithmetic_op<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;

    Ok(match (lhst, rhst) {
        (TypeDecl::Any, _) => TypeDecl::Any,
        (_, TypeDecl::Any) => TypeDecl::Any,
        (TypeDecl::Bool, TypeDecl::Bool) => TypeDecl::Bool,
        (TypeDecl::Num, TypeDecl::Num) => TypeDecl::Num,
        (TypeDecl::Int, TypeDecl::Int) => TypeDecl::Int,
        (TypeDecl::Str, TypeDecl::Str) => TypeDecl::Str,
        _ => {
            return Err(TypeCheckError::new(
                format!(
                    "Operator '{}' cannot be applied to types '{}' and '{}'.",
                    op, lhst, rhst,
                ),
                lhs.span,
            ))
        }
    })
}

// <, <=, >, >=
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
        (Num, Num) => Bool,
        (Int, Int) => Bool,
        (Num, Int) => Bool,
        (Int, Num) => Bool,
        (Str, Str) => Bool,
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

// ==, !=
fn tc_binary_ee<'src>(
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
    ctx: &mut TypeCheckContext<'src, '_>,
    op: &str,
) -> Result<TypeDecl, TypeCheckError<'src>> {
    use TypeDecl::*;
    let lhst = tc_expr(lhs, ctx)?;
    let rhst = tc_expr(rhs, ctx)?;
    Ok(match (&lhst, &rhst) {
        (Any, _) => Bool,
        (_, Any) => Bool,
        (Undefined, _) => Bool,
        (_, Undefined) => Bool,
        (Null, _) => Bool,
        (_, Null) => Bool,
        (Num, Num) => Bool,
        (Int, Int) => Bool,
        (Num, Int) => Bool,
        (Int, Num) => Bool,
        (Str, Str) => Bool,
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
        Ident(str) => ctx.vars.get(**str).map(|v| v.td).ok_or_else(|| {
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
                tc_coerce_type(arg_ty, &decl.1, *arg_span)?;
            }
            func.ret_type()
        }
        Not(_ex) => TypeDecl::Bool,
        BwNot(ex) => {
            if tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Int, ex.span).is_err() {
                tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Num, ex.span)?;
            }
            TypeDecl::Num
        }
        Minus(ex) => {
            if tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Int, ex.span).is_err() {
                tc_coerce_type(&tc_expr(ex, ctx)?, &TypeDecl::Num, ex.span)?;
            }
            TypeDecl::Num
        }
        Plus(ex) => {
            let td = tc_expr(ex, ctx)?;
            if tc_coerce_type(&td, &TypeDecl::Num, ex.span).is_err() {
                return Err(TypeCheckError::new(
                    format!("Operator '+' cannot be applied to type '{td}'."),
                    ex.span,
                ));
            }
            TypeDecl::Num
        }

        Add(lhs, rhs) => tc_bin_add_op(lhs, rhs, ctx)?,
        Sub(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "-")?,
        Mul(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "*")?,
        Div(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "/")?,
        Mod(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "%")?,
        BwAnd(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "&")?,
        BwOr(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "|")?,
        BwXor(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "^")?,
        BwLShift(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, "<<")?,
        BwRShift(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, ">>")?,
        BwRShiftU(lhs, rhs) => tc_bin_arithmetic_op(lhs, rhs, ctx, ">>>")?,

        Lt(lhs, rhs) => tc_binary_cmp(lhs, rhs, ctx, "<")?,
        Le(lhs, rhs) => tc_binary_cmp(lhs, rhs, ctx, "<=")?,
        Gt(lhs, rhs) => tc_binary_cmp(lhs, rhs, ctx, ">")?,
        Ge(lhs, rhs) => tc_binary_cmp(lhs, rhs, ctx, ">=")?,
        Ee(lhs, rhs) => tc_binary_ee(lhs, rhs, ctx, "==")?,
        Ne(lhs, rhs) => tc_binary_ee(lhs, rhs, ctx, "!=")?,
        Eee(_lhs, _rhs) => Ok(TypeDecl::Bool)?,
        Nee(_lhs, _rhs) => Ok(TypeDecl::Bool)?,
        Ternary {
            cond,
            true_branch,
            false_branch,
        } => {
            let _ = tc_expr(cond, ctx)?;

            let true_type = tc_expr(true_branch, ctx)?;
            let false_type = tc_expr(false_branch, ctx)?;
            tc_coerce_type(&true_type, &false_type, true_branch.span)?;
            true_type
        }
        Await(ex) => {
            let _res = tc_expr(ex, ctx)?;
            TypeDecl::Any
        }
        Spread(_ex) => unreachable!("Spread operator should be handled in parser"),
        Entry(_key, _val) => unreachable!("Entry should be handled in parser"),
        Satisfies(ex, td) => {
            let res = tc_expr(ex, ctx)?;
            if tc_coerce_type(&res, td, ex.span).is_err() {
                return Err(TypeCheckError::new(
                    format!("Type '{res}' does not satisfy the expected type '{td}'."),
                    ex.span,
                ));
            }
            res
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
            Statement::Null => {}
            Statement::Import { span, .. } => {
                return Err(TypeCheckError::new(
                    "import statement is not yet supported.".into(),
                    *span,
                ));
            }
            Statement::ImportType { .. } => {
                // TODO
            }
            Statement::VarDef {
                name,
                td,
                ex,
                is_var,
                is_const,
                ..
            } => {
                if *is_var {
                    return Err(TypeCheckError::new(
                        "Keyword 'var' is not supported. Use 'let' or 'const' instead.".into(),
                        *name,
                    ));
                }
                let mut init_type = tc_expr(ex, ctx)?;
                if let Some(td) = td {
                    init_type = tc_coerce_type(&init_type, td, ex.span)?;
                }
                ctx.vars.insert(
                    **name,
                    VarDecl {
                        td: init_type,
                        is_const: *is_const,
                    },
                );
            }
            Statement::VarAssign { name, ex, .. } => {
                let init_type = tc_expr(ex, ctx)?;

                let Some(target) = ctx.vars.get(**name) else {
                    return Err(TypeCheckError::new(
                        format!("Variable '{}' not found in scope", name),
                        *name,
                    ));
                };
                if target.is_const {
                    return Err(TypeCheckError::new(
                        format!("Cannot assign to '{}' because it is a constant.", name),
                        *name,
                    ));
                }

                tc_coerce_type(&init_type, &target.td, ex.span)?;
            }
            Statement::Block(stmts) => {
                res = type_check(stmts, ctx)?;
            }
            Statement::If {
                cond,
                true_branch,
                false_branch,
            } => {
                let _ = tc_expr(cond, ctx)?;

                let Statement::Block(true_branch) = &**true_branch else {
                    unreachable!("If statement should have a block in true branch");
                };
                let true_type = type_check(true_branch, ctx)?;

                if let Some(false_branch) = false_branch {
                    match &**false_branch {
                        Statement::Block(_) => {
                            type_check(&vec![*false_branch.clone()], ctx)?;
                        }
                        Statement::If { .. } => {
                            type_check(&vec![*false_branch.clone()], ctx)?;
                        }
                        _ => {
                            unreachable!("If statement should have a block in false branch, but got {false_branch:?}");
                        }
                    }
                }
                res = true_type;
            }

            Statement::FnDef {
                name,
                args,
                ret_type,
                stmts,
                is_cofn,
            } => {
                let ret_type = if let Some(ret_type) = ret_type {
                    *ret_type
                } else {
                    TypeDecl::Any
                };

                // Function declaration needs to be added first to allow recursive calls
                ctx.funcs.insert(
                    name.to_string(),
                    FnDecl::User(UserFn::new(args.clone(), ret_type, *is_cofn)),
                );
                let mut subctx = TypeCheckContext::push_stack(ctx);
                for (arg, ty) in args.iter() {
                    subctx.vars.insert(
                        arg,
                        VarDecl {
                            td: *ty,
                            is_const: false,
                        },
                    );
                }
                let last_stmt = type_check(stmts, &mut subctx)?;
                tc_coerce_type(&last_stmt, &ret_type, stmts.span())?;
            }
            Statement::Expression(e) => {
                res = tc_expr(e, ctx)?;
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
