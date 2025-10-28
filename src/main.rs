mod sexpr;
use pest::Parser;
use sexpr::{SExpr, Rule, L2Parser};

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
