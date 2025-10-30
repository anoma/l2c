use pest::Parser;
use crate::sexpr::{SExpr, Rule, L2Parser};

pub const LITERAL_LEN: usize = 32;

#[derive(Debug, Clone)]
pub struct MetaExpr {
    pub fragment: SExpr,
    pub reference: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct SymbolExpr {
    pub reference: String,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub value: u32,
}

#[derive(Debug, Clone)]
pub struct WithExpr {
    pub reference: String,
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionExpr {
    pub reference: String,
    pub parameters: Vec<String>,
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ContinuationExpr {
    pub reference: String,
    pub parameters: Vec<String>,
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct InvokeExpr {
    pub reference: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct JumpExpr {
    pub reference: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct StorageExpr {
    pub reference: String,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub consequent: Box<Expr>,
    pub alternate: Box<Expr>,
}

#[derive(Debug, Clone)]
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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Symbol(sym) => write!(f, "{}", sym.reference),
            Self::Literal(value) => write!(f, "{}", value.value),
            Self::With(with) => write!(f, "(with {} {})", with.reference, with.expression),
            Self::Function(function) => {
                write!(f, "(function {} (", function.reference)?;
                if let [param0, rest @ ..] = &function.parameters[..] {
                    write!(f, "{}", param0)?;
                    for param in rest {
                        write!(f, " {}", param)?;
                    }
                }
                write!(f, ") {})", function.expression)
            },
            Self::Continuation(continuation) => {
                write!(f, "(continuation {} (", continuation.reference)?;
                if let [param0, rest @ ..] = &continuation.parameters[..] {
                    write!(f, "{}", param0)?;
                    for param in rest {
                        write!(f, " {}", param)?;
                    }
                }
                write!(f, ") {})", continuation.expression)
            },
            Self::Invoke(invoke) => {
                write!(f, "[{}", invoke.reference)?;
                for arg in &invoke.arguments {
                    write!(f, " {}", arg)?;
                }
                write!(f, "]")
            },
            Self::Jump(invoke) => {
                write!(f, "{{{}", invoke.reference)?;
                for arg in &invoke.arguments {
                    write!(f, " {}", arg)?;
                }
                write!(f, "}}")
            },
            Self::Storage(storage) => {
                write!(f, "(storage {}", storage.reference)?;
                for arg in &storage.arguments {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            },
            Self::If(ife) => write!(f, "(if {} {} {})", ife.condition, ife.consequent, ife.alternate),
            Self::Meta(meta) => write!(f, "{}", meta.fragment),
        }
    }
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
        [keyword, ..] => {
            Expr::Meta(MetaExpr {
                reference: Box::new(parse_expr(keyword.clone())?),
                fragment: SExpr::List(sexpr),
            })
        },
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
