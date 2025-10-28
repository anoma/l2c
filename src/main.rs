use pest::Parser;
use pest_derive::Parser;
use pest::iterators::Pair;
#[derive(Parser)]
#[grammar = "l2.pest"]
pub struct L2Parser;

#[derive(Debug, Clone)]
pub enum SExpr {
    Char(char),
    List(Vec<SExpr>),
}

impl SExpr {
    pub fn make_symbol(string: &str) -> SExpr {
        SExpr::List(string.chars().map(|ch| SExpr::Char(ch)).collect())
    }

    pub fn parse_token(pair: Pair<Rule>) -> SExpr {
        assert_eq!(pair.as_rule(), Rule::token);
        Self::make_symbol(pair.as_str())
    }

    pub fn parse_list(pair: Pair<Rule>, prefix: Option<SExpr>) -> SExpr {
        assert!(pair.as_rule() == Rule::list || pair.as_rule() == Rule::slist || pair.as_rule() == Rule::clist);
        let mut fragments: Vec<_> = prefix.into_iter().collect();
        for fragment in pair.into_inner() {
            assert_eq!(fragment.as_rule(), Rule::fragment);
            fragments.push(Self::parse_fragment(fragment));
        }
        SExpr::List(fragments)
    }

    pub fn parse_fragment2(pair: Pair<Rule>) -> SExpr {
        assert_eq!(pair.as_rule(), Rule::fragment2);
        let mut pairs = pair.into_inner();
        let pair = pairs
            .next()
            .expect("fragment2 should be non-empty");
        assert_eq!(pairs.next(), None);
        match pair.as_rule() {
            Rule::token => Self::parse_token(pair),
            Rule::list => Self::parse_list(pair, None),
            Rule::clist => Self::parse_list(pair, Some(Self::make_symbol("jump"))),
            Rule::slist => Self::parse_list(pair, Some(Self::make_symbol("invoke"))),
            _ => unreachable!("fragment2 element should be a list or token")
        }
    }

    pub fn parse_fragment1(pair: Pair<Rule>) -> SExpr {
        assert_eq!(pair.as_rule(), Rule::fragment1);
        let mut fragments = vec![];
        for fragment in pair.into_inner() {
            assert_eq!(fragment.as_rule(), Rule::fragment2);
            fragments.push(Self::parse_fragment2(fragment));
        }
        if fragments.len() == 1 {
            fragments.remove(0)
        } else {
            SExpr::List(fragments)
        }
    }

    pub fn parse_fragment(pair: Pair<Rule>) -> SExpr {
        assert_eq!(pair.as_rule(), Rule::fragment);
        let mut fragments = vec![];
        for fragment in pair.into_inner() {
            assert_eq!(fragment.as_rule(), Rule::fragment1);
            fragments.push(Self::parse_fragment1(fragment));
        }
        if fragments.len() == 1 {
            fragments.remove(0)
        } else {
            SExpr::List(fragments)
        }
    }
}

impl std::fmt::Display for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self::List(lst) = self else {
            panic!("cannot display a character S-expression")
        };
        match lst[..] {
            [] => write!(f, "()")?,
            [Self::Char(_), ..] => {
                for ch in lst {
                    let Self::Char(ch) = ch else { panic!("must be character") };
                    write!(f, "{}", ch)?;
                }
            },
            _ => {
                let mut iter = lst.iter();
                write!(f, "({}", iter.next().expect("list should be non-empty"))?;
                while let Some(fragment) = iter.next() {
                    write!(f, " {}", fragment)?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

pub fn parse_program(source: &str) -> Result<Vec<SExpr>, Box<pest::error::Error<Rule>>> {
    let program = L2Parser::parse(Rule::program, source)?;
    let mut fragments = vec![];
    for fragment in program {
        match fragment.as_rule() {
            Rule::fragment => fragments.push(SExpr::parse_fragment(fragment)),
            Rule::EOI => return Ok(fragments),
            _ => unreachable!("program element should be fragment or EOI"),
        }
    }
    unreachable!("EOI should have occured")
}

fn main() {
    println!("Program: {}", parse_program("{hello world}").unwrap()[0]);
}
