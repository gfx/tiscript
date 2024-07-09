use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while_m_n},
    character::complete::{alpha1, alphanumeric1, char, digit1},
    combinator::{cut, map, map_opt, map_res, opt, recognize, value, verify},
    error::ParseError,
    multi::{fold_many0, many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, InputTake, Offset, Parser,
};

use crate::ast::{ExprEnum, Expression, Span, Statement, Statements, TypeDecl};

pub trait GetSpan<'a> {
    fn span(&self) -> Span<'a>;
}

impl<'a> GetSpan<'a> for Statements<'a> {
    fn span(&self) -> Span<'a> {
        self.iter().find_map(|stmt| stmt.span()).unwrap()
    }
}

pub fn multispace0<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(many0(alt((
        line_comment,
        block_comment,
        nom::character::complete::multispace1,
    ))))(input)
}

pub fn multispace1<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(many1(alt((
        line_comment,
        block_comment,
        nom::character::complete::multispace1,
    ))))(input)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<Span<'src>, O, E>,
) -> impl FnMut(Span<'src>) -> IResult<Span<'src>, O, E>
where
    E: ParseError<Span<'src>>,
{
    delimited(multispace0, f, multispace0)
}

/// Calculate offset between the start positions of the input spans and return a span between them.
///
/// Note: `i` shall start earlier than `r`, otherwise wrapping would occur.
pub(crate) fn calc_offset<'a>(i: Span<'a>, r: Span<'a>) -> Span<'a> {
    i.take(i.offset(&r))
}

fn invoke_fn<'a>(name: Span<'a>, args: Vec<Expression<'a>>, span: Span<'a>) -> Expression<'a> {
    Expression::new(ExprEnum::FnInvoke(name, args), span)
}

fn shebang(i: Span) -> IResult<Span, ()> {
    let (r, _) = tag("#!")(i)?;
    let (r, _) = take_until("\n")(r)?;
    Ok((r, ()))
}

// JavaScript-style line comments (// ...)
fn line_comment<'a, E: ParseError<Span<'a>>>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>, E> {
    let (r, _) = tag("//")(i)?;
    let (r, _) = take_until("\n")(r)?;
    Ok((r, i))
}
// JavaScript-style block comments (/* ... */), not nestable.
fn block_comment<'a, E: ParseError<Span<'a>>>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>, E> {
    let (r, _) = tag("/*")(i)?;
    let (r, _) = take_until("*/")(r)?;
    let (r, _) = tag("*/")(r)?;
    Ok((r, i))
}

fn unary_op(i: Span) -> IResult<Span, Expression> {
    let (r, op) = opt(space_delimited(alt((tag("!"), tag("-"), tag("+")))))(i)?;
    let (r, ex) = space_delimited(factor)(r)?;

    if let Some(op) = op {
        return Ok((
            r,
            Expression {
                expr: match *op.fragment() {
                    "!" => ExprEnum::Not(Box::new(ex)),
                    "-" => ExprEnum::Minus(Box::new(ex)),
                    "+" => ExprEnum::Plus(Box::new(ex)),
                    _ => unreachable!(),
                },
                span: i,
            },
        ));
    } else {
        Ok((r, ex))
    }
}

fn factor(i: Span) -> IResult<Span, Expression> {
    alt((
        undefined_literal,
        null_literal,
        true_literal,
        false_literal,
        dq_str_literal,
        sq_str_literal,
        tmpl_str_literal,
        bigint_literal,
        num_literal,
        array_literal,
        object_literal,
        func_call,
        ident,
        parens,
    ))(i)
}

fn func_call(i: Span) -> IResult<Span, Expression> {
    let (r, ident) = space_delimited(identifier)(i)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
        tag(")"),
    ))(r)?;
    Ok((
        r,
        Expression {
            expr: ExprEnum::FnInvoke(ident, args),
            span: i,
        },
    ))
}

fn ident(input: Span) -> IResult<Span, Expression> {
    let (r, res) = space_delimited(identifier)(input)?;
    Ok((
        r,
        Expression {
            expr: ExprEnum::Ident(res),
            span: input,
        },
    ))
}

fn identifier(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        // . is not a ECMA-262 spec but for a tentative solution to call class methods.
        alt((alpha1, tag("_"), tag("$"))),
        many0(alt((alphanumeric1, tag("_"), tag("$"), tag(".")))),
    ))(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StrFragment<'a> {
    Literal(Span<'a>),
    EscapedChar(char),
}

fn parse_unicode(input: Span<'_>) -> IResult<Span<'_>, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = preceded(char('u'), delimited(char('{'), parse_hex, char('}')));

    let parse_u32 = map_res(parse_delimited_hex, move |hex: Span| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    map_opt(parse_u32, std::char::from_u32)(input)
}

fn parse_escaped_char(input: Span<'_>) -> IResult<Span<'_>, char> {
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
            value('\'', char('\'')),
        )),
    )(input)
}

fn parse_dq_literal(input: Span<'_>) -> IResult<Span, Span> {
    let not_quote_slash = is_not("\"\\");
    verify(not_quote_slash, |s: &Span| !s.is_empty())(input)
}

fn parse_dq_fragment(input: Span<'_>) -> IResult<Span<'_>, StrFragment<'_>> {
    alt((
        // The `map` combinator runs a parser, then applies a function to the output
        // of that parser.
        map(parse_dq_literal, StrFragment::Literal),
        map(parse_escaped_char, StrFragment::EscapedChar),
    ))(input)
}

fn parse_sq_literal(input: Span<'_>) -> IResult<Span, Span> {
    let not_quote_slash = is_not("\'\\");
    verify(not_quote_slash, |s: &Span| !s.is_empty())(input)
}

fn parse_sq_fragment(input: Span<'_>) -> IResult<Span<'_>, StrFragment<'_>> {
    alt((
        map(parse_sq_literal, StrFragment::Literal),
        map(parse_escaped_char, StrFragment::EscapedChar),
    ))(input)
}

fn parse_tmpl_literal(input: Span<'_>) -> IResult<Span, Span> {
    let not_quote_slash = is_not("`\\");
    verify(not_quote_slash, |s: &Span| !s.is_empty())(input)
}

fn parse_tmpl_fragment(input: Span<'_>) -> IResult<Span<'_>, StrFragment<'_>> {
    alt((
        map(parse_tmpl_literal, StrFragment::Literal),
        map(parse_escaped_char, StrFragment::EscapedChar),
    ))(input)
}

/// parse a double-quoted string literal
fn dq_str_literal(i: Span) -> IResult<Span, Expression> {
    let build_string = fold_many0(parse_dq_fragment, String::new, |mut string, fragment| {
        match fragment {
            StrFragment::Literal(s) => string.push_str(s.fragment()),
            StrFragment::EscapedChar(c) => string.push(c),
        }
        string
    });

    let (r, val) = delimited(char('"'), build_string, char('"'))(i)?;
    Ok((r, Expression::new(ExprEnum::StrLiteral(val), i)))
}

/// parse a single-quoted string literal
fn sq_str_literal(i: Span) -> IResult<Span, Expression> {
    let build_string = fold_many0(parse_sq_fragment, String::new, |mut string, fragment| {
        match fragment {
            StrFragment::Literal(s) => string.push_str(s.fragment()),
            StrFragment::EscapedChar(c) => string.push(c),
        }
        string
    });

    let (r, val) = delimited(char('\''), build_string, char('\''))(i)?;
    Ok((r, Expression::new(ExprEnum::StrLiteral(val), i)))
}

/// parse a template string literal
// TODO: implement interpolation of expressions (`${expr}`).
fn tmpl_str_literal(i: Span) -> IResult<Span, Expression> {
    let build_string = fold_many0(parse_tmpl_fragment, String::new, |mut string, fragment| {
        match fragment {
            StrFragment::Literal(s) => string.push_str(s.fragment()),
            StrFragment::EscapedChar(c) => string.push(c),
        }
        string
    });

    let (r, val) = delimited(char('`'), build_string, char('`'))(i)?;
    Ok((r, Expression::new(ExprEnum::StrLiteral(val), i)))
}

fn num_literal(input: Span) -> IResult<Span, Expression> {
    let (r, v) = space_delimited(recognize(pair(
        alt((
            map(
                tuple((
                    pair(digit1, many0(alt((digit1, tag("_"))))),
                    opt(pair(
                        char('.'),
                        opt(pair(digit1, many0(alt((digit1, tag("_")))))),
                    )),
                )),
                |_| (),
            ),
            map(
                tuple((char('.'), pair(digit1, many0(alt((digit1, tag("_"))))))),
                |_| (),
            ),
        )),
        opt(tuple((
            alt((char('e'), char('E'))),
            opt(alt((char('+'), char('-')))),
            cut(pair(digit1, many0(alt((digit1, tag("_")))))),
        ))),
    )))(input)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::NumLiteral(v.replace('_', "").parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input,
                    code: nom::error::ErrorKind::Digit,
                })
            })?),
            v,
        ),
    ))
}

// a bigint literal is an integer literal with `n` suffix such as `0n`, `123n`.
// each digit can be separated by `_` such as `1_000_000n`.
fn bigint_literal(input: Span) -> IResult<Span, Expression> {
    let (r, v) = space_delimited(recognize(tuple((
        digit1,
        many0(alt((digit1, tag("_")))),
        char('n'),
    ))))(input)?;
    let digits = v.fragment().trim_end_matches('n').replace('_', "");
    Ok((
        r,
        Expression::new(
            ExprEnum::BigIntLiteral(digits.parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input,
                    code: nom::error::ErrorKind::Digit,
                })
            })?),
            v,
        ),
    ))
}

fn undefined_literal(input: Span) -> IResult<Span, Expression> {
    let (r, _) = space_delimited(tag("undefined"))(input)?;
    Ok((r, Expression::new(ExprEnum::UndefinedLiteral, input)))
}

fn null_literal(input: Span) -> IResult<Span, Expression> {
    let (r, _) = space_delimited(tag("null"))(input)?;
    Ok((r, Expression::new(ExprEnum::NullLiteral, input)))
}

fn true_literal(input: Span) -> IResult<Span, Expression> {
    let (r, _) = space_delimited(tag("true"))(input)?;
    Ok((r, Expression::new(ExprEnum::BoolLiteral(true), input)))
}

fn false_literal(input: Span) -> IResult<Span, Expression> {
    let (r, _) = space_delimited(tag("false"))(input)?;
    Ok((r, Expression::new(ExprEnum::BoolLiteral(false), input)))
}

// the spread operator works like a unary operator
fn spread_expr(input: Span) -> IResult<Span, Expression> {
    let (r, (_, ex)) = space_delimited(pair(tag("..."), expr))(input)?;
    Ok((r, Expression::new(ExprEnum::Spread(Box::new(ex)), input)))
}

fn array_literal(input: Span) -> IResult<Span, Expression> {
    let (r, list) = space_delimited(delimited(
        char('['),
        many0(delimited(
            multispace0,
            alt((expr, spread_expr)),
            space_delimited(opt(char(','))),
        )),
        char(']'),
    ))(input)?;

    let array_of = Span::new("Array.of");

    // if a spread expr exists, transform it into Array.spread%(), otherwise Array.of().
    if list.iter().any(|ex| matches!(ex.expr, ExprEnum::Spread(_))) {
        // e.g. [1, 2, ...expr1, 3, 4, ...expr2]
        let mut subarrays: Vec<Expression> = Vec::new(); // e.g. [[1, 2], [3, 4]]
        let mut spreadings: Vec<Expression> = Vec::new(); // e.g. [expr1, expr2]
        let mut temp: Vec<Expression> = Vec::new();

        for ex in list {
            match ex.expr {
                ExprEnum::Spread(ex) => {
                    subarrays.push(invoke_fn(array_of, temp, ex.span));
                    spreadings.push(*ex);
                    temp = Vec::new();
                }
                _ => {
                    temp.push(ex);
                }
            }
        }
        if !temp.is_empty() {
            let span = temp[0].span;
            subarrays.push(invoke_fn(array_of, temp, span));
        }

        Ok((
            r,
            invoke_fn(
                Span::new("Array.spread%"),
                vec![
                    invoke_fn(array_of, subarrays, input),
                    invoke_fn(array_of, spreadings, input),
                ],
                input,
            ),
        ))
    } else {
        Ok((r, invoke_fn(array_of, list, input)))
    }
}

fn object_literal(input: Span) -> IResult<Span, Expression> {
    // { identifier: expr, "string": expr, ... }
    let (r, pairs) = space_delimited(delimited(
        char('{'),
        many0(delimited(
            multispace0,
            pair(
                alt((ident, dq_str_literal, sq_str_literal)),
                preceded(
                    multispace0,
                    preceded(char(':'), preceded(multispace0, expr)),
                ),
            ),
            space_delimited(opt(char(','))),
        )),
        char('}'),
    ))(input)?;
    let array_of = Span::new("Array.of");

    // make Vec<(Expression, Expression)> to Vec<Expression> where each element is Array.of(k, v) in the latter.
    let list = pairs
        .into_iter()
        .map(|(k, v)| {
            let k = match k {
                Expression {
                    expr: ExprEnum::Ident(s),
                    span: _,
                } => Expression {
                    expr: ExprEnum::StrLiteral(s.to_string()),
                    span: k.span,
                },
                Expression {
                    expr: ExprEnum::StrLiteral(_),
                    span: _,
                } => k,
                _ => unreachable!(),
            };
            let span = k.span;
            invoke_fn(array_of, vec![k, v], span)
        })
        .collect();

    Ok((r, invoke_fn(Span::new("Object.fromEntries"), list, input)))
}

fn parens(i: Span) -> IResult<Span, Expression> {
    space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn term(i: Span) -> IResult<Span, Expression> {
    let (r, init) = unary_op(i)?;

    let res = fold_many0(
        pair(
            space_delimited(alt((char('*'), char('/'), char('%')))),
            unary_op,
        ),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(i, acc.span);
            match op {
                '*' => Expression::new(ExprEnum::Mul(Box::new(acc), Box::new(val)), span),
                '/' => Expression::new(ExprEnum::Div(Box::new(acc), Box::new(val)), span),
                '%' => Expression::new(ExprEnum::Mod(Box::new(acc), Box::new(val)), span),
                _ => panic!("Multiplicative expression should have '*', '%', or '/' operator"),
            }
        },
    )(r);
    res
}

fn add_expr(i: Span) -> IResult<Span, Expression> {
    let (r, init) = term(i)?;

    let res = fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(i, acc.span);
            match op {
                '+' => Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span),
                '-' => Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span),
                _ => panic!("Additive expression should have '+' or '-' operator"),
            }
        },
    )(r);
    res
}

fn cond_expr(i0: Span) -> IResult<Span, Expression> {
    let (i, first) = add_expr(i0)?;
    let (i, cond) = space_delimited(alt((char('<'), char('>'))))(i)?;
    let (i, second) = add_expr(i)?;
    let span = calc_offset(i0, i);
    Ok((
        i,
        match cond {
            '<' => Expression::new(ExprEnum::Lt(Box::new(first), Box::new(second)), span),
            '>' => Expression::new(ExprEnum::Gt(Box::new(first), Box::new(second)), span),
            // TODO: ==, !=, ===, !===, <=, >=
            _ => unreachable!(),
        },
    ))
}

fn open_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

fn close_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(char('}'))(i)?;
    Ok((i, ()))
}

fn if_expr(i0: Span) -> IResult<Span, Expression> {
    let (i, _) = space_delimited(tag("if"))(i0)?;
    let (i, cond) = expr(i)?;
    let (i, t_case) = delimited(open_brace, statements, close_brace)(i)?;
    let (i, f_case) = opt(preceded(
        space_delimited(tag("else")),
        alt((
            delimited(open_brace, statements, close_brace),
            map_res(
                if_expr,
                |v| -> Result<Vec<Statement>, nom::error::Error<&str>> {
                    Ok(vec![Statement::Expression(v)])
                },
            ),
        )),
    ))(i)?;

    Ok((
        i,
        Expression::new(
            ExprEnum::If(Box::new(cond), Box::new(t_case), f_case.map(Box::new)),
            calc_offset(i0, i),
        ),
    ))
}

fn await_expr(i: Span) -> IResult<Span, Expression> {
    let i0 = i;
    let (i, _) = space_delimited(tag("await"))(i)?;
    let (i, ex) = cut(space_delimited(expr))(i)?;
    Ok((
        i,
        Expression::new(ExprEnum::Await(Box::new(ex)), calc_offset(i0, i)),
    ))
}

fn expr(i: Span) -> IResult<Span, Expression> {
    alt((await_expr, if_expr, cond_expr, add_expr))(i)
}

fn var_def<'a>(span: Span<'a>, i: Span<'a>, is_const: bool) -> IResult<Span<'a>, Statement<'a>> {
    let (i, (name, td, ex)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;

        // type declaration is optional
        let mut td: Option<TypeDecl> = None;
        let (mut i, r) = opt(space_delimited(char(':')))(i)?;
        if r.is_some() {
            let actual_td;
            (i, actual_td) = type_decl(i)?;
            td = Some(actual_td);
        }

        let (i, _) = space_delimited(char('='))(i)?;
        let (i, ex) = space_delimited(expr)(i)?;
        let (i, _) = space_delimited(char(';'))(i)?;
        Ok((i, (name, td, ex)))
    })(i)?;
    Ok((
        i,
        Statement::VarDef {
            span: calc_offset(span, i),
            name,
            td,
            ex,
            is_const,
        },
    ))
}

fn let_def(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, _) = delimited(multispace0, tag("let"), multispace1)(i)?;
    var_def(span, i, false)
}

fn const_def(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, _) = delimited(multispace0, tag("const"), multispace1)(i)?;
    var_def(span, i, true)
}

fn var_assign(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(char(';'))(i)?;
    Ok((
        i,
        Statement::VarAssign {
            span: calc_offset(span, i),
            name,
            ex,
        },
    ))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn type_decl(i: Span) -> IResult<Span, TypeDecl> {
    let (i, td) = space_delimited(identifier)(i)?;
    Ok((
        i,
        match *td.fragment() {
            "null" => TypeDecl::Null,
            "bigint" => TypeDecl::Int,
            "number" => TypeDecl::Num,
            "string" => TypeDecl::Str,
            "cofn" => TypeDecl::Coro,
            _ => {
                return Err(nom::Err::Failure(nom::error::Error::new(
                    td,
                    nom::error::ErrorKind::Verify,
                )));
            }
        },
    ))
}

fn argument(i: Span) -> IResult<Span, (Span, TypeDecl)> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = char(':')(i)?;
    let (i, td) = type_decl(i)?;

    Ok((i, (ident, td)))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
    let (i, fn_kw) = space_delimited(alt((tag("cofn"), tag("function"))))(i)?;
    let (i, (name, args, ret_type, stmts)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;
        let (i, _) = space_delimited(tag("("))(i)?;
        let (i, args) = separated_list0(char(','), space_delimited(argument))(i)?;
        let (i, _) = space_delimited(tag(")"))(i)?;
        let (i, _) = space_delimited(tag("->"))(i)?;
        let (i, ret_type) = type_decl(i)?;
        let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
        Ok((i, (name, args, ret_type, stmts)))
    })(i)?;
    Ok((
        i,
        Statement::FnDef {
            name,
            args,
            ret_type,
            stmts,
            is_cofn: *fn_kw == "cofn",
        },
    ))
}

fn export_default_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("export"))(i)?;
    let (i, _) = space_delimited(tag("default"))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;
    Ok((i, Statement::ExportDefault(ex)))
}

fn export_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("export"))(i)?;
    let (i, stmt) = statement(i)?;
    // check the statement includes either a variable definition or a function definition.
    match &stmt {
        Statement::VarDef { .. } | Statement::FnDef { .. } => {}
        _ => {
            return Err(nom::Err::Failure(nom::error::Error::new(
                i,
                nom::error::ErrorKind::Verify,
            )));
        }
    }
    Ok((i, Statement::Export(vec![stmt])))
}

fn return_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;
    Ok((i, Statement::Return(ex)))
}

fn yield_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("yield"))(i)?;
    let (i, ex) = cut(space_delimited(expr))(i)?;
    Ok((i, Statement::Yield(ex)))
}

fn general_statement<'a>(last: bool) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Statement> {
    let terminator = move |i| -> IResult<Span, ()> {
        let mut semicolon = pair(tag(";"), multispace0);
        if last {
            Ok((opt(semicolon)(i)?.0, ()))
        } else {
            Ok((semicolon(i)?.0, ()))
        }
    };
    move |input| {
        alt((
            let_def,
            const_def,
            var_assign,
            fn_def_statement,
            return_statement,
            yield_statement,
            export_default_statement,
            export_statement,
            terminated(expr_statement, terminator),
        ))(input)
    }
}

pub(crate) fn last_statement(input: Span) -> IResult<Span, Statement> {
    let (input, _) = many0(alt((line_comment, block_comment)))(input)?;
    general_statement(true)(input)
}

pub(crate) fn statement(input: Span) -> IResult<Span, Statement> {
    let (input, _) = many0(alt((line_comment, block_comment)))(input)?;
    general_statement(false)(input)
}

fn statements(i: Span) -> IResult<Span, Statements> {
    let (i, _) = opt(shebang)(i)?;
    let (i, mut stmts) = many0(statement)(i)?;
    let (i, last) = opt(last_statement)(i)?;
    let (i, _) = many0(alt((multispace1, line_comment, block_comment)))(i)?;
    if let Some(last) = last {
        stmts.push(last);
    }
    Ok((i, stmts))
}

pub fn statements_finish(i: Span) -> Result<Statements, nom::error::Error<Span>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_num_literal_simple() {
        let input = Span::new("3.14");
        let (_r, ex) = num_literal(input).unwrap();
        assert_eq!(ex.expr, ExprEnum::NumLiteral(3.14));
    }

    #[test]
    fn test_num_literal_w_underscores() {
        let input = Span::new("1_000_000.000_000");
        let (_r, ex) = num_literal(input).unwrap();
        assert_eq!(ex.expr, ExprEnum::NumLiteral(1_000_000.0));
    }

    #[test]
    fn test_bigint_literal_0n() {
        let input = Span::new("0n");
        let (_r, ex) = bigint_literal(input).unwrap();
        assert_eq!(ex.expr, ExprEnum::BigIntLiteral(0));
    }

    #[test]
    fn test_bigint_literal_1n() {
        let input = Span::new("1n");
        let (_r, ex) = bigint_literal(input).unwrap();
        assert_eq!(ex.expr, ExprEnum::BigIntLiteral(1));
    }

    #[test]
    fn test_bigint_literal_w_underscores() {
        let input = Span::new("1_000_000n");
        let (_r, ex) = bigint_literal(input).unwrap();
        assert_eq!(ex.expr, ExprEnum::BigIntLiteral(1_000_000));
    }

    #[test]
    fn test_unary_op_not() {
        let input = Span::new("!true");
        let (_r, ex) = unary_op(input).unwrap();
        assert_eq!(*ex.span.fragment(), "!true");
    }

    #[test]
    fn test_unary_op_plus() {
        let input = Span::new("+3");
        let (_r, ex) = unary_op(input).unwrap();
        assert_eq!(*ex.span.fragment(), "+3");
    }

    #[test]
    fn test_unary_op_minus() {
        let input = Span::new("-3");
        let (_r, ex) = unary_op(input).unwrap();
        assert_eq!(*ex.span.fragment(), "-3");
    }

    #[test]
    fn test_parse_sq_literal() {
        let input = Span::new("'abc'");
        let (_r, ex) = sq_str_literal(input).unwrap();
        assert_eq!(ex.expr, ExprEnum::StrLiteral("abc".to_string()));
    }

    #[test]
    fn test_array_spread() {
        let input = Span::new("[1, 2, ...arr, 3, 4]");
        let r = array_literal(input);
        eprintln!("{r:?}");
        assert!(r.is_ok());
    }
}
