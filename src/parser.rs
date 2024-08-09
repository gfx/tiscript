use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while_m_n},
    character::complete::{alpha1, alphanumeric1, char, digit1},
    combinator::{cut, map, map_opt, map_res, opt, recognize, value, verify},
    error::ParseError,
    multi::{fold_many0, many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    Finish, IResult, InputTake, Offset, Parser,
};

use crate::ast::{ExprEnum, Expression, Span, Statement, Statements, TypeDecl};

pub type Error<'a> = nom::error::Error<Span<'a>>;

pub trait GetSpan<'a> {
    fn span(&self) -> Span<'a>;
}

impl<'a> GetSpan<'a> for Statements<'a> {
    fn span(&self) -> Span<'a> {
        self.iter()
            .find_map(|stmt| stmt.span())
            .or_else(|| Some(Span::new("")))
            .unwrap()
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

// end of statement: semicolon or newline
fn eos(input: Span) -> IResult<Span, char> {
    alt((
        space_delimited(char(';')),
        preceded(multispace0, char('\n')),
    ))(input)
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
    let (r, op) = opt(space_delimited(alt((
        tag("!"),
        tag("~"),
        tag("-"),
        tag("+"),
    ))))(i)?;
    let (r, ex) = space_delimited(factor)(r)?;

    if let Some(op) = op {
        return Ok((
            r,
            Expression {
                expr: match *op.fragment() {
                    "!" => ExprEnum::Not(Box::new(ex)),
                    "~" => ExprEnum::BwNot(Box::new(ex)),
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

fn factor(input: Span) -> IResult<Span, Expression> {
    let (i, expr) = alt((
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
    ))(input)?;

    let Ok((i, _)) = tag::<&str, Span, Error>("**")(i) else {
        return Ok((i, expr));
    };

    let (i, rhs) = space_delimited(factor)(i)?;
    Ok((
        i,
        Expression {
            expr: ExprEnum::Pow(Box::new(expr), Box::new(rhs)),
            span: input,
        },
    ))
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

#[derive(Debug, Clone, PartialEq)]
enum StrFragment<'a> {
    Literal(Span<'a>),
    EscapedChar(char),
    Interpolation(Expression<'a>),
}

fn parse_unicode(input: Span) -> IResult<Span, char> {
    let parse_hex1_6 = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_hex4 = take_while_m_n(4, 4, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = preceded(
        char('u'),
        alt((delimited(char('{'), parse_hex1_6, char('}')), parse_hex4)),
    );

    let parse_u32 = map_res(parse_delimited_hex, move |hex: Span| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    map_opt(parse_u32, std::char::from_u32)(input)
}

fn parse_hex_escape(input: Span) -> IResult<Span, char> {
    let parse_hex = take_while_m_n(2, 2, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = preceded(char('x'), parse_hex);

    let parse_u8 = map_res(parse_delimited_hex, move |hex: Span| {
        u32::from_str_radix(hex.fragment(), 16)
    });

    map_opt(parse_u8, std::char::from_u32)(input)
}

fn parse_escaped_char(input: Span<'_>) -> IResult<Span<'_>, char> {
    preceded(
        char('\\'),
        alt((
            parse_hex_escape,
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

fn parse_interpolation(input: Span<'_>) -> IResult<Span<'_>, Expression> {
    let (i, _) = char('$')(input)?;
    let (i, _) = char('{')(i)?;
    let (i, ex) = expr(i)?;
    let (i, _) = char('}')(i)?;
    Ok((i, ex))
}

fn parse_dq_literal(input: Span<'_>) -> IResult<Span, Span> {
    let not_quote_slash = is_not("\"\\");
    verify(not_quote_slash, |s: &Span| !s.is_empty())(input)
}

fn parse_dq_fragment(input: Span<'_>) -> IResult<Span<'_>, StrFragment<'_>> {
    alt((
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
    let not_quote_slash = is_not("`\\$");
    verify(not_quote_slash, |s: &Span| !s.is_empty())(input)
}

fn parse_tmpl_fragment(input: Span<'_>) -> IResult<Span<'_>, StrFragment<'_>> {
    alt((
        map(parse_tmpl_literal, StrFragment::Literal),
        map(parse_escaped_char, StrFragment::EscapedChar),
        map(parse_interpolation, StrFragment::Interpolation),
    ))(input)
}

/// parse a double-quoted string literal
fn dq_str_literal(i: Span) -> IResult<Span, Expression> {
    let build_string = fold_many0(parse_dq_fragment, String::new, |mut string, fragment| {
        match fragment {
            StrFragment::Literal(s) => string.push_str(s.fragment()),
            StrFragment::EscapedChar(c) => string.push(c),
            _ => unreachable!(),
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
            _ => unreachable!(),
        }
        string
    });

    let (r, val) = delimited(char('\''), build_string, char('\''))(i)?;
    Ok((r, Expression::new(ExprEnum::StrLiteral(val), i)))
}

// template literals
fn tmpl_str_literal(i: Span) -> IResult<Span, Expression> {
    // transform `foo ${expr1} bar ${expr2}` into `foo ` + expr1 + ` bar ` + expr2
    let build_tmpl = fold_many0(
        parse_tmpl_fragment,
        || Expression::new(ExprEnum::StrLiteral("".into()), i),
        |lhs, fragment| match fragment {
            StrFragment::Literal(s) => Expression::new(
                ExprEnum::Add(
                    Box::new(lhs),
                    Box::new(Expression::new(
                        ExprEnum::StrLiteral(s.fragment().to_string()),
                        i,
                    )),
                ),
                i,
            ),
            StrFragment::EscapedChar(c) => Expression::new(
                ExprEnum::Add(
                    Box::new(lhs),
                    Box::new(Expression::new(ExprEnum::StrLiteral(c.to_string()), i)),
                ),
                i,
            ),
            StrFragment::Interpolation(rhs) => {
                Expression::new(ExprEnum::Add(Box::new(lhs), Box::new(rhs)), i)
            }
        },
    );

    let (r, expr) = delimited(char('`'), build_tmpl, char('`'))(i)?;
    Ok((r, expr))
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
                nom::Err::Error(Error {
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
                nom::Err::Error(Error {
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

fn object_entry(input: Span) -> IResult<Span, Expression> {
    let (r, (k, v)) = pair(
        alt((ident, dq_str_literal, sq_str_literal)),
        preceded(
            multispace0,
            preceded(char(':'), preceded(multispace0, expr)),
        ),
    )(input)?;
    Ok((
        r,
        Expression::new(ExprEnum::Entry(Box::new(k), Box::new(v)), input),
    ))
}

fn object_literal(input: Span) -> IResult<Span, Expression> {
    // { identifier: expr, "string": expr, ...spread }
    let (r, entries) = space_delimited(delimited(
        char('{'),
        many0(delimited(
            multispace0,
            alt((object_entry, spread_expr)),
            space_delimited(opt(char(','))),
        )),
        char('}'),
    ))(input)?;
    let array_of = Span::new("Array.of");
    let object_from_entries = Span::new("Object.fromEntries");

    if entries
        .iter()
        .any(|ex| matches!(ex.expr, ExprEnum::Spread(_)))
    {
        // transform object spreads into use of Object.assign()
        // e.g. { a: b, ...c, d: e, ...f } -> Object.assign(Object.fromEntries([[a, b]], c, Object.fromEntries([[ d: e ]]), f)
        let mut args = Vec::new();
        let mut temp = Vec::new(); // each arg for Object.fromEntries()

        for ex in entries {
            match ex.expr {
                ExprEnum::Spread(ex) => {
                    if !temp.is_empty() {
                        let entries = invoke_fn(array_of, temp, ex.span);
                        args.push(invoke_fn(object_from_entries, vec![entries], ex.span));
                        temp = Vec::new();
                    }
                    args.push(*ex);
                }
                ExprEnum::Entry(k, v) => {
                    let k = match *k {
                        Expression {
                            expr: ExprEnum::Ident(s),
                            ..
                        } => Expression {
                            expr: ExprEnum::StrLiteral(s.to_string()),
                            span: k.span,
                        },
                        Expression {
                            expr: ExprEnum::StrLiteral(_),
                            ..
                        } => *k,
                        _ => unreachable!(),
                    };

                    temp.push(invoke_fn(array_of, vec![k, *v], ex.span));
                }
                _ => unreachable!(),
            }
        }
        if !temp.is_empty() {
            let entries = invoke_fn(array_of, temp, input);
            args.push(invoke_fn(object_from_entries, vec![entries], input));
        }

        Ok((r, invoke_fn(Span::new("Object.assign"), args, input)))
    } else {
        // transform object literal into use of Object.fromEntries()
        let entries = entries
            .into_iter()
            .map(|entry| match entry {
                Expression {
                    expr: ExprEnum::Entry(k, v),
                    ..
                } => {
                    let k = match *k {
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
                        } => *k,
                        _ => unreachable!(),
                    };

                    let span = k.span;
                    invoke_fn(array_of, vec![k, *v], span)
                }
                _ => {
                    unreachable!("object_entry should return an Entry")
                }
            })
            .collect();

        Ok((
            r,
            invoke_fn(
                object_from_entries,
                vec![invoke_fn(array_of, entries, input)],
                input,
            ),
        ))
    }
}

fn parens(i: Span) -> IResult<Span, Expression> {
    space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn multiply_op(i: Span) -> IResult<Span, Expression> {
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
    let (r, init) = multiply_op(i)?;

    let res = fold_many0(
        pair(
            space_delimited(alt((
                tag("+"),
                tag("-"),
                tag("|"),
                tag("&"),
                tag("^"),
                tag("<<"),
                tag(">>>"),
                tag(">>"),
            ))),
            multiply_op,
        ),
        move || init.clone(),
        |acc, (op, val)| {
            let span = calc_offset(i, acc.span);
            match *op.fragment() {
                "+" => Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span),
                "-" => Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span),
                "|" => Expression::new(ExprEnum::BwOr(Box::new(acc), Box::new(val)), span),
                "&" => Expression::new(ExprEnum::BwAnd(Box::new(acc), Box::new(val)), span),
                "^" => Expression::new(ExprEnum::BwXor(Box::new(acc), Box::new(val)), span),
                "<<" => Expression::new(ExprEnum::BwLShift(Box::new(acc), Box::new(val)), span),
                ">>" => Expression::new(ExprEnum::BwRShift(Box::new(acc), Box::new(val)), span),
                ">>>" => Expression::new(ExprEnum::BwRShiftU(Box::new(acc), Box::new(val)), span),
                _ => unreachable!("Additive expression should have '+' or '-' operator"),
            }
        },
    )(r);
    res
}

fn cmp_expr(i0: Span) -> IResult<Span, Expression> {
    let (i, first) = add_expr(i0)?;
    let (i, cond) = space_delimited(alt((
        tag("<="),
        tag("<"),
        tag(">="),
        tag(">"),
        tag("!=="),
        tag("!="),
        tag("==="),
        tag("=="),
    )))(i)?;
    let (i, second) = add_expr(i)?;
    let span = calc_offset(i0, i);
    Ok((
        i,
        match *cond.fragment() {
            "<" => Expression::new(ExprEnum::Lt(Box::new(first), Box::new(second)), span),
            "<=" => Expression::new(ExprEnum::Le(Box::new(first), Box::new(second)), span),
            ">" => Expression::new(ExprEnum::Gt(Box::new(first), Box::new(second)), span),
            ">=" => Expression::new(ExprEnum::Ge(Box::new(first), Box::new(second)), span),
            "==" => Expression::new(ExprEnum::Ee(Box::new(first), Box::new(second)), span),
            "!=" => Expression::new(ExprEnum::Ne(Box::new(first), Box::new(second)), span),
            "===" => Expression::new(ExprEnum::Eee(Box::new(first), Box::new(second)), span),
            "!==" => Expression::new(ExprEnum::Nee(Box::new(first), Box::new(second)), span),
            _ => unreachable!(),
        },
    ))
}

fn satisfies_expr(input: Span) -> IResult<Span, Expression> {
    let (i, ex) = multiply_op(input)?;
    let (i, _) = space_delimited(tag("satisfies"))(i)?;
    let (i, td) = cut(space_delimited(type_expr))(i)?;
    Ok((
        i,
        Expression::new(ExprEnum::Satisfies(Box::new(ex), td), calc_offset(input, i)),
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

fn expr(input: Span) -> IResult<Span, Expression> {
    let (i, ex) = alt((await_expr, satisfies_expr, cmp_expr, add_expr))(input)?;

    let Ok((i, _)) = char::<Span, Error>('?')(i) else {
        return Ok((i, ex));
    };

    // ternary operator (expr ? expr : expr)
    let (i, true_branch) = expr(i)?;
    let (i, _) = space_delimited(char(':'))(i)?;
    let (i, false_branch) = expr(i)?;
    Ok((
        i,
        Expression::new(
            ExprEnum::Ternary {
                cond: Box::new(ex),
                true_branch: Box::new(true_branch),
                false_branch: Box::new(false_branch),
            },
            calc_offset(input, i),
        ),
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

fn block_statement(i: Span) -> IResult<Span, Statement> {
    let (i, stmts) = delimited(open_brace, |input| statements(input, false), close_brace)(i)?;
    Ok((i, Statement::Block(stmts)))
}

fn if_statement(input: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("if"))(input)?;
    let (i, cond) = expr(i)?;
    let (i, true_branch) = block_statement(i)?;
    let (i, false_branch) = opt(preceded(
        space_delimited(tag("else")),
        alt((block_statement, if_statement)),
    ))(i)?;

    Ok((
        i,
        Statement::If {
            cond: Box::new(cond),
            true_branch: Box::new(true_branch),
            false_branch: false_branch.map(Box::new),
        },
    ))
}

#[allow(dead_code) /* enum fields are not yet used */]
#[derive(Debug, Clone, PartialEq)]
enum ImportSpecifier<'a> {
    Default(Span<'a>),              // import foo
    Namespace(Span<'a>),            // import * as foo
    Named(Span<'a>),                // import { foo }
    NamedAlias(Span<'a>, Span<'a>), // import { foo as bar }
}

fn import_default(input: Span) -> IResult<Span, Vec<ImportSpecifier>> {
    let (i, name) = space_delimited(identifier)(input)?;
    Ok((i, vec![ImportSpecifier::Default(name)]))
}

fn import_namespace(input: Span) -> IResult<Span, Vec<ImportSpecifier>> {
    let (i, _) = space_delimited(tag("*"))(input)?;
    let (i, _) = space_delimited(tag("as"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    Ok((i, vec![ImportSpecifier::Namespace(name)]))
}

fn import_named(input: Span) -> IResult<Span, Vec<ImportSpecifier>> {
    let (i, (first, rest)) = delimited(
        space_delimited(tag("{")),
        pair(
            pair(
                space_delimited(identifier),
                opt(preceded(
                    space_delimited(tag("as")),
                    space_delimited(identifier),
                )),
            ),
            many0(preceded(
                space_delimited(tag(",")),
                pair(
                    space_delimited(identifier),
                    opt(preceded(
                        space_delimited(tag("as")),
                        space_delimited(identifier),
                    )),
                ),
            )),
        ),
        space_delimited(tag("}")),
    )(input)?;

    let mut list = Vec::new();

    let (name, alias) = first;
    if let Some(alias) = alias {
        list.push(ImportSpecifier::NamedAlias(name, alias));
    } else {
        list.push(ImportSpecifier::Named(name));
    }

    for (name, alias) in rest {
        if let Some(alias) = alias {
            list.push(ImportSpecifier::NamedAlias(name, alias));
        } else {
            list.push(ImportSpecifier::Named(name));
        }
    }

    Ok((i, list))
}

// IMPORT_LIST from MODULE_SPECIFIER;
fn import_list_from_module(input: Span) -> IResult<Span, Span> {
    // import type IMPORT_LIST from "module";

    let (i, _) = alt((import_default, import_namespace, import_named))(input)?;

    let (i, _) = space_delimited(tag("from"))(i)?;
    let (i, _) = space_delimited(alt((dq_str_literal, sq_str_literal)))(i)?;
    let (i, _) = eos(i)?;

    Ok((i, input))
}

fn import_def(input: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("import"))(input)?;
    let (i, _) = import_list_from_module(i)?;
    Ok((i, Statement::Import { span: input }))
}

fn import_type_def(input: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("import"))(input)?;
    let (i, _) = space_delimited(tag("type"))(i)?;
    let (i, _) = import_list_from_module(i)?;
    Ok((i, Statement::ImportType { span: input }))
}

fn variable_def<'a>(
    span: Span<'a>,
    i: Span<'a>,
    is_const: bool,
    is_var: bool,
) -> IResult<Span<'a>, Statement<'a>> {
    let (i, (name, td, init)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;

        // type declaration is optional
        let mut td: Option<TypeDecl> = None;
        let (mut i, r) = opt(space_delimited(char(':')))(i)?;
        if r.is_some() {
            let actual_td;
            (i, actual_td) = type_expr(i)?;
            td = Some(actual_td);
        }

        let (i, init) = opt(|input| {
            let (i, _) = space_delimited(char('='))(input)?;
            space_delimited(expr)(i)
        })(i)?;
        let (i, _) = eos(i)?;
        Ok((i, (name, td, init)))
    })(i)?;
    Ok((
        i,
        Statement::VarDef {
            span: calc_offset(span, i),
            name,
            td,
            init,
            is_const,
            is_var,
        },
    ))
}

fn var_def(input: Span) -> IResult<Span, Statement> {
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(input)?;
    variable_def(input, i, false, true)
}

fn let_def(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, _) = delimited(multispace0, tag("let"), multispace1)(i)?;
    variable_def(span, i, false, false)
}

fn const_def(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, _) = delimited(multispace0, tag("const"), multispace1)(i)?;
    variable_def(span, i, true, false)
}

fn var_assign(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, ex) = space_delimited(expr)(i)?;
    let (i, _) = eos(i)?;
    Ok((
        i,
        Statement::VarAssign {
            span: calc_offset(span, i),
            name,
            ex,
        },
    ))
}

fn type_def(input: Span) -> IResult<Span, Statement> {
    // type foo = bar;
    let (i, _) = space_delimited(tag("type"))(input)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, td) = space_delimited(type_expr)(i)?;
    let (i, _) = eos(i)?;
    Ok((i, Statement::Type { name, td }))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, res) = expr(i)?;
    let (i, _) = eos(i)?;
    Ok((i, Statement::Expression(res)))
}

fn type_primitive(i: Span) -> IResult<Span, TypeDecl> {
    let (i, td) = space_delimited(identifier)(i)?;

    Ok((
        i,
        match *td.fragment() {
            "undefined" => TypeDecl::Undefined,
            "null" => TypeDecl::Null,
            "boolean" => TypeDecl::Bool,
            "number" => TypeDecl::Num,
            "bigint" => TypeDecl::Int,
            "string" => TypeDecl::Str,
            _ => TypeDecl::Any,
        },
    ))
}

fn type_object(input: Span) -> IResult<Span, TypeDecl> {
    // { key: value, "key": "value", ... }
    // almost the same as object literals, but no arbitrary expressions.
    let (i, _) = space_delimited(delimited(
        open_brace,
        many0(delimited(
            multispace0,
            alt((object_entry, spread_expr)),
            space_delimited(opt(char(','))),
        )),
        close_brace,
    ))(input)?;
    Ok((i, TypeDecl::Object))
}

fn type_expr(i: Span) -> IResult<Span, TypeDecl> {
    let (i, td) = alt((type_primitive, type_object))(i)?;
    Ok((i, td))
}

fn argument(i: Span) -> IResult<Span, (Span, TypeDecl)> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = char(':')(i)?;
    let (i, td) = type_expr(i)?;

    Ok((i, (ident, td)))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
    let (i, fn_kw) = space_delimited(alt((tag("cofn"), tag("function"))))(i)?;
    let (i, (name, args, ret_type, stmts)) = cut(|i| {
        let (i, name) = space_delimited(identifier)(i)?;
        let (i, _) = space_delimited(tag("("))(i)?;
        let (i, args) = separated_list0(char(','), space_delimited(argument))(i)?;
        let (mut i, _) = space_delimited(tag(")"))(i)?;

        let ret_type: Option<TypeDecl>;
        if let Ok((ii, _)) = space_delimited(char::<Span, Error>(':'))(i) {
            let (ii, td) = type_expr(ii)?;
            i = ii;
            ret_type = Some(td);
        } else {
            ret_type = None;
        }
        let (i, stmts) = delimited(open_brace, |input| statements(input, false), close_brace)(i)?;
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
    let (i, _) = eos(i)?;
    Ok((i, Statement::ExportDefault(ex)))
}

fn export_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("export"))(i)?;
    let (i, stmt) = statement(i)?;
    // check the statement includes either a variable definition or a function definition.
    match &stmt {
        Statement::VarDef { .. } | Statement::FnDef { .. } => {}
        _ => {
            return Err(nom::Err::Error(Error::new(
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
    let (i, _) = eos(i)?;
    Ok((i, Statement::Return(ex)))
}

fn yield_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("yield"))(i)?;
    let (i, ex) = cut(space_delimited(expr))(i)?;
    let (i, _) = eos(i)?;
    Ok((i, Statement::Yield(ex)))
}

fn null_statement(input: Span) -> IResult<Span, Statement> {
    let (i, _) = eos(input)?;
    Ok((i, Statement::Null))
}

fn statement(input: Span) -> IResult<Span, Statement> {
    alt((
        import_type_def,
        import_def,
        var_def,
        let_def,
        const_def,
        var_assign,
        type_def,
        if_statement,
        fn_def_statement,
        return_statement,
        yield_statement,
        export_default_statement,
        export_statement,
        expr_statement,
        null_statement,
    ))(input)
}

fn statements(input: Span, main: bool) -> IResult<Span, Statements> {
    let i = if main {
        let (input, _) = opt(shebang)(input)?;
        input
    } else {
        input
    };
    let (i, stmts) = many0(statement)(i)?;
    let (i, _) = many0(alt((multispace1, line_comment, block_comment)))(i)?;
    Ok((i, stmts))
}

pub fn statements_finish(i: Span) -> Result<Statements, Error> {
    let (_, res) = statements(i, true).finish()?;
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
    fn test_parse_interpolation1() {
        let input = Span::new("${1 - 2}");
        let (_r, ex) = parse_interpolation(input).unwrap();
        assert!(matches!(ex.expr, ExprEnum::Sub(..)),);
    }

    #[test]
    fn test_parse_interpolation2() {
        let input = Span::new("${hello}");
        let (_r, ex) = parse_interpolation(input).unwrap();
        assert!(matches!(ex.expr, ExprEnum::Ident(..)),);
    }

    #[test]
    fn test_array_spread() {
        let input = Span::new("[1, 2, ...arr, 3, 4]");
        let r = array_literal(input);
        eprintln!("{r:?}");
        assert!(r.is_ok());
    }

    #[test]
    fn test_object_spread() {
        let input = Span::new("{ foo: 42, ...other }");
        let r = object_literal(input);
        eprintln!("{r:?}");
        assert!(r.is_ok());
    }

    #[test]
    fn test_cmp_expr() {
        let input = Span::new("1 <= 2");
        let (_r, ex) = cmp_expr(input).unwrap();
        assert_eq!(*ex.span.fragment(), "1 <= 2");
    }

    #[test]
    fn test_good_escape() {
        let input = Span::new(r#""\x00""#);
        let r = parse_escaped_char(input);
        assert!(r.is_err());
    }

    #[test]
    fn test_bad_escape1() {
        let input = Span::new(r#""\x""#);
        let r = parse_escaped_char(input);
        assert!(r.is_err());
    }

    #[test]
    fn test_bad_escape2() {
        let input = Span::new(r#""\x0""#);
        let r = parse_escaped_char(input);
        assert!(r.is_err());
    }

    #[test]
    fn test_bad_escape3() {
        let input = Span::new(r#""\x000""#);
        let r = parse_escaped_char(input);
        assert!(r.is_err());
    }

    #[test]
    fn test_type_object_empty() {
        let input = Span::new("{}");
        let r = type_object(input);
        assert!(r.is_ok());
    }

    #[test]
    fn test_type_object_entries() {
        let input = Span::new("{ foo: string, bar: number }");
        let r = type_object(input);
        assert!(r.is_ok());
    }

    #[test]
    fn test_statement() {
        let input = Span::new("let x = 42;");
        let r = statement(input);
        assert!(r.is_ok());
    }

    #[test]
    fn test_statement_w_whitespaces() {
        let input = Span::new("\n let x = 42; \n");
        let r = statement(input);
        assert!(r.is_ok());
    }

    #[test]
    fn test_statements() {
        let input = Span::new("let x = 42; let y = 3.14;");
        let r = statement(input);
        assert!(r.is_ok());
    }

    #[test]
    fn test_statements_w_whitespaces() {
        let input = Span::new(
            r#"
            let x = 42;
            let y = 3.14;
        "#,
        );
        let r = statement(input);
        assert!(r.is_ok());
    }

    #[test]
    fn test_null_statement() {
        let input = Span::new(r#";"#);
        let r = statement(input);
        assert!(r.is_ok());
    }
}
