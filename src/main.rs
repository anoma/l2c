mod sexpr;
use pest::Parser;
use sexpr::{SExpr, Rule, L2Parser};

pub const LITERAL_LEN: usize = 32;

#[derive(Debug)]
pub enum Expr {
    Function(FunctionExpr),
    Continuation(ContinuationExpr),
    With(WithExpr),
    Symbol(SymbolExpr),
    Meta(MetaExpr),
    Literal(LiteralExpr),
    If(IfExpr),
    Invoke(InvokeExpr),
    Jump(JumpExpr),
    Storage(StorageExpr),
}

#[derive(Debug)]
pub struct MetaExpr {
    fragment: SExpr,
}

#[derive(Debug)]
pub struct SymbolExpr {
    reference: String,
}

#[derive(Debug)]
pub struct LiteralExpr {
    value: u32,
}

#[derive(Debug)]
pub struct WithExpr {
    reference: String,
    expression: Box<Expr>,
}

#[derive(Debug)]
pub struct FunctionExpr {
    reference: String,
    parameters: Vec<String>,
    expression: Box<Expr>,
}

#[derive(Debug)]
pub struct ContinuationExpr {
    reference: String,
    parameters: Vec<String>,
    expression: Box<Expr>,
}

#[derive(Debug)]
pub struct InvokeExpr {
    reference: Box<Expr>,
    arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct JumpExpr {
    reference: Box<Expr>,
    arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct StorageExpr {
    reference: String,
    arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct IfExpr {
    condition: Box<Expr>,
    consequent: Box<Expr>,
    alternate: Box<Expr>,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedString(SExpr),
    ExpectedList(SExpr),
    UnexpectedLiteral(String),
    SExprError(pest::error::Error<Rule>),
}

pub fn parse_string(sexpr: SExpr) -> Result<String, ParserError> {
    let SExpr::List(sexpr) = sexpr else {
        return Err(ParserError::ExpectedString(sexpr));
    };
    let mut chars = vec![];
    for elt in &sexpr {
        let SExpr::Char(ch) = elt else {
            return Err(ParserError::ExpectedString(SExpr::List(sexpr)));
        };
        chars.push(*ch);
    }
    Ok(chars.iter().collect())
}

pub fn is_symbol(sexpr: &SExpr) -> bool {
    let SExpr::List(sexpr) = sexpr else {
        return false;
    };
    if sexpr.is_empty() {
        return false;
    }
    for elt in sexpr {
        let SExpr::Char(ch) = elt else {
            return false;
        };
    }
    true
}

pub fn is_keyword(sexpr: &SExpr, keyword: &str) -> bool {
    let SExpr::List(chars) = sexpr else {
        return false;
    };
    if chars.len() != keyword.len() {
        return false;
    }
    for (a, b) in chars.iter().zip(keyword.chars()) {
        if a != &SExpr::Char(b) {
            return false;
        }
    }
    true
}

pub fn parse_expr(sexpr: SExpr) -> Result<Expr, ParserError> {
    let SExpr::List(sexpr) = sexpr else {
        panic!("S-expression must be a list");
    };
    Ok(match &sexpr[..] {
        [] => panic!("list must be non-empty"),
        [SExpr::Char(_), ..] => {
            Expr::Symbol(SymbolExpr {
                reference: parse_string(SExpr::List(sexpr.clone()))?,
            })
        },
        [keyword, value] if is_keyword(keyword, "literal") => {
            let string = parse_string(value.clone())?;
            if string.len() != LITERAL_LEN {
                return Err(ParserError::UnexpectedLiteral(string));
            }
            let value = u32::from_str_radix(&string, 2).map_err(|_| ParserError::UnexpectedLiteral(string))?;
            Expr::Literal(LiteralExpr {
                value,
            })
        },
        [keyword, reference, expr] if is_keyword(keyword, "with") => {
            Expr::With(WithExpr {
                reference: parse_string(reference.clone())?,
                expression: Box::new(parse_expr(expr.clone())?),
            })
        },
        [keyword, reference, parameters, expr] if is_keyword(keyword, "function") => {
            let SExpr::List(parameters) = parameters else {
                return Err(ParserError::ExpectedList(parameters.clone()));
            };
            Expr::Function(FunctionExpr {
                reference: parse_string(reference.clone())?,
                parameters: parameters.iter().map(|x| parse_string(x.clone())).collect::<Result<_, _>>()?,
                expression: Box::new(parse_expr(expr.clone())?),
            })
        },
        [keyword, reference, parameters, expr] if is_keyword(keyword, "continuation") => {
            let SExpr::List(parameters) = parameters else {
                return Err(ParserError::ExpectedList(parameters.clone()));
            };
            Expr::Continuation(ContinuationExpr {
                reference: parse_string(reference.clone())?,
                parameters: parameters.iter().map(|x| parse_string(x.clone())).collect::<Result<_, _>>()?,
                expression: Box::new(parse_expr(expr.clone())?),
            })
        },
        [keyword, reference, arguments @ ..] if is_keyword(keyword, "invoke") => {
            Expr::Invoke(InvokeExpr {
                reference: Box::new(parse_expr(reference.clone())?),
                arguments: arguments.iter().map(|x| parse_expr(x.clone())).collect::<Result<_, _>>()?,
            })
        },
        [keyword, reference, arguments @ ..] if is_keyword(keyword, "jump") => {
            Expr::Jump(JumpExpr {
                reference: Box::new(parse_expr(reference.clone())?),
                arguments: arguments.iter().map(|x| parse_expr(x.clone())).collect::<Result<_, _>>()?,
            })
        },
        [keyword, reference, arguments @ ..] if is_keyword(keyword, "storage") => {
            Expr::Storage(StorageExpr {
                reference: parse_string(reference.clone())?,
                arguments: arguments.iter().map(|x| parse_expr(x.clone())).collect::<Result<_, _>>()?,
            })
        },
        [keyword, condition, consequent, alternate] if is_keyword(keyword, "if") => {
            Expr::If(IfExpr {
                condition: Box::new(parse_expr(condition.clone())?),
                consequent: Box::new(parse_expr(consequent.clone())?),
                alternate: Box::new(parse_expr(alternate.clone())?),
            })
        },
        _ => Expr::Meta(MetaExpr { fragment: SExpr::List(sexpr) }),
    })
}

pub fn parse_program(source: &str) -> Result<Vec<Expr>, ParserError> {
    let program = L2Parser::parse(Rule::program, source).map_err(|x| ParserError::SExprError(x))?;
    let mut fragments = vec![];
    for fragment in program {
        match fragment.as_rule() {
            Rule::fragment => fragments.push(parse_expr(SExpr::parse_fragment(fragment))?),
            Rule::EOI => return Ok(fragments),
            _ => unreachable!("program element should be fragment or EOI"),
        }
    }
    unreachable!("EOI should have occured")
}

fn main() {
    println!("Program: {:?}", parse_program("{hello [world]}").unwrap());
}
