mod sexpr;
mod expr;
mod transpile;

fn main() {
    //let program = expr::parse_program("(function hi (aa) [+ (with hello (if a {hello [world 55]} [car])) 56])").unwrap();
    //let program = expr::parse_program("(function hi (aa) (storage hey 1 2 9))").unwrap();
    //let program = expr::parse_program("(function factorial (n) (if [== n 0] 1 [* n [factorial [- n 1]]]))").unwrap();
    let program = expr::parse_program(
        "(function factorial (n) (with ret {(continuation loop (m acc) (if [== m 0] {ret acc} {loop [- m 1] [* m acc]})) n 1}))"
    ).unwrap();
    let mut transpiler = transpile::Transpiler::default();
    println!("Source Program:");
    for expr in program {
        println!("{}", expr);
        transpiler.transpile(expr, (&transpile::CExpr::Literal(0), &transpile::CTypeName::new("void".to_string())), &mut Vec::new());
    }
    println!();
    println!("Compiled Outputs:");
    println!("{}", transpiler);
}
