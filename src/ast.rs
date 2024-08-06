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
    Entry(Box<Expression<'src>>, Box<Expression<'src>>),
    Spread(Box<Expression<'src>>),
    FnInvoke(Span<'src>, Vec<Expression<'src>>),
    Not(Box<Expression<'src>>),
    BwNot(Box<Expression<'src>>),
    Minus(Box<Expression<'src>>),
    Plus(Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Mod(Box<Expression<'src>>, Box<Expression<'src>>),
    BwOr(Box<Expression<'src>>, Box<Expression<'src>>),
    BwAnd(Box<Expression<'src>>, Box<Expression<'src>>),
    BwXor(Box<Expression<'src>>, Box<Expression<'src>>),
    BwLShift(Box<Expression<'src>>, Box<Expression<'src>>),
    BwRShift(Box<Expression<'src>>, Box<Expression<'src>>),
    BwRShiftU(Box<Expression<'src>>, Box<Expression<'src>>),
    Lt(Box<Expression<'src>>, Box<Expression<'src>>),
    Le(Box<Expression<'src>>, Box<Expression<'src>>),
    Gt(Box<Expression<'src>>, Box<Expression<'src>>),
    Ge(Box<Expression<'src>>, Box<Expression<'src>>),
    Ee(Box<Expression<'src>>, Box<Expression<'src>>),
    Ne(Box<Expression<'src>>, Box<Expression<'src>>),
    Eee(Box<Expression<'src>>, Box<Expression<'src>>),
    Nee(Box<Expression<'src>>, Box<Expression<'src>>),
    Ternary {
        cond: Box<Expression<'src>>,
        true_branch: Box<Expression<'src>>,
        false_branch: Box<Expression<'src>>,
    },
    Satisfies(Box<Expression<'src>>, TypeDecl),
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
    Null,
    Expression(Expression<'src>),
    Import {
        span: Span<'src>,
        // TODO
    },
    ImportType {
        span: Span<'src>,
        // TODO
    },
    VarDef {
        span: Span<'src>,
        name: Span<'src>,
        td: Option<TypeDecl>,
        init: Option<Expression<'src>>,
        is_const: bool,
        is_var: bool,
    },
    VarAssign {
        span: Span<'src>,
        name: Span<'src>,
        ex: Expression<'src>,
    },
    Block(Statements<'src>),
    If {
        cond: Box<Expression<'src>>,
        true_branch: Box<Statement<'src>>,
        false_branch: Option<Box<Statement<'src>>>,
    },
    FnDef {
        name: Span<'src>,
        args: Vec<(Span<'src>, TypeDecl)>,
        ret_type: Option<TypeDecl>,
        stmts: Statements<'src>,
        is_cofn: bool,
    },
    Return(Expression<'src>),
    Yield(Expression<'src>),
    ExportDefault(Expression<'src>),
    Export(Statements<'src>), // stmts.len() == 1

    Type {
        name: Span<'src>,
        td: TypeDecl,
    },
}

impl<'src> Statement<'src> {
    pub fn span(&self) -> Option<Span<'src>> {
        match self {
            Statement::Null => None,
            Statement::Expression(ex) => Some(ex.span),
            Statement::Import { span, .. } => Some(*span),
            Statement::ImportType { span, .. } => Some(*span),
            Statement::VarDef { span, .. } => Some(*span),
            Statement::VarAssign { span, .. } => Some(*span),
            // safely unwrap a Option<Option<T>>
            Statement::Block(stmts) => stmts.first().map(|stmt| stmt.span()).unwrap_or(None),
            Statement::If { cond, .. } => Some(cond.span),
            Statement::FnDef { name, stmts, .. } => Some(calc_offset(*name, stmts.span())),
            Statement::Return(ex) => Some(ex.span),
            Statement::Yield(ex) => Some(ex.span),
            Statement::ExportDefault(ex) => Some(ex.span),
            Statement::Export(stmts) => stmts[0].span(),
            Statement::Type { name, .. } => Some(*name),
        }
    }
}
pub type Statements<'a> = Vec<Statement<'a>>;
