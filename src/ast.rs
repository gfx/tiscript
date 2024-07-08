use crate::parser::{calc_offset, GetSpan};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeDecl {
    Any,
    Undefined,
    Null,
    Bool,
    Num,
    Int,
    Str,
    Array,
    Object,
    Coro,
}

impl std::fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeDecl::*;
        write!(
            f,
            "{}",
            match self {
                Any => "any",
                Undefined => "undefined",
                Null => "null",
                Bool => "boolean",
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

#[derive(Debug, PartialEq, Clone)]
pub enum ExprEnum<'src> {
    UndefinedLiteral,
    NullLiteral,
    BoolLiteral(bool),
    Ident(Span<'src>),
    NumLiteral(f64),
    BigIntLiteral(i64),
    StrLiteral(String),
    Spread(Box<Expression<'src>>),
    FnInvoke(Span<'src>, Vec<Expression<'src>>),
    Not(Box<Expression<'src>>),
    Minus(Box<Expression<'src>>),
    Plus(Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Mod(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Statements<'src>>,
        Option<Box<Statements<'src>>>,
    ),
    Await(Box<Expression<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<'a> {
    pub(crate) expr: ExprEnum<'a>,
    pub(crate) span: Span<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(expr: ExprEnum<'a>, span: Span<'a>) -> Self {
        Self { expr, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef {
        span: Span<'src>,
        name: Span<'src>,
        td: Option<TypeDecl>,
        ex: Expression<'src>,
        is_const: bool,
    },
    VarAssign {
        span: Span<'src>,
        name: Span<'src>,
        ex: Expression<'src>,
    },
    FnDef {
        name: Span<'src>,
        args: Vec<(Span<'src>, TypeDecl)>,
        ret_type: TypeDecl,
        stmts: Statements<'src>,
        is_cofn: bool,
    },
    Return(Expression<'src>),
    Yield(Expression<'src>),
    ExportDefault(Expression<'src>),
    Export(Statements<'src>), // stmts.len() == 1
}

impl<'src> Statement<'src> {
    pub fn span(&self) -> Option<Span<'src>> {
        use Statement::*;
        Some(match self {
            Expression(ex) => ex.span,
            VarDef { span, .. } => *span,
            VarAssign { span, .. } => *span,
            FnDef { name, stmts, .. } => calc_offset(*name, stmts.span()),
            Return(ex) => ex.span,
            Yield(ex) => ex.span,
            ExportDefault(ex) => ex.span,
            Export(stmts) => stmts[0].span().unwrap(),
        })
    }
}

pub type Statements<'a> = Vec<Statement<'a>>;
