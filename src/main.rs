mod sexpr;
mod expr;
mod transpile;
mod expand;
use std::collections::HashMap;
use std::rc::Rc;

fn main() {
    //let program = expr::parse_program("(function hi (aa) [+ (with hello (if a {hello [world 55]} [car])) 56])").unwrap();
    //let program = expr::parse_program("(function hi (aa) (storage hey 1 2 9))").unwrap();
    //let program = expr::parse_program("[(function factorial (n) (if [== n (literal 00000000000000000000000000000000)] (literal 00000000000000000000000000000001) [* n [factorial [- n (literal 00000000000000000000000000000001)]]])) (literal 00000000000000000000000000000101)]").unwrap();
    let mut program = expr::parse_program("((function _ (x) [car [cdr x]]) [(function factorial (n) (if [== n (literal 00000000000000000000000000000000)] (literal 00000000000000000000000000000001) [* n [factorial [- n (literal 00000000000000000000000000000001)]]])) (literal 00000000000000000000000000000101)])").unwrap();
    /*let program = expr::parse_program(
        "[(function factorial (n) (with ret {(continuation loop (m acc) (if [== m (literal 00000000000000000000000000000000)] {ret acc} {loop [- m (literal 00000000000000000000000000000001)] [* m acc]})) n (literal 00000000000000000000000000000001)})) (literal 00000000000000000000000000000011)]"
    ).unwrap();*/
    println!("Source Program:");
    for expr in &program {
        println!("{}", expr);
    }
    println!();
    println!("Expanded Program:");
    for expr in &mut program {
        let result = expand::eval(expand::expand(expr), 0, &HashMap::new(), Rc::new(expand::identity)).unwrap();
        println!("{}", expr);
        println!("Result: {}", result);
    }
    println!();
    println!("Compiled Outputs:");
    let mut transpiler = transpile::Transpiler::default();
    for expr in program {
        transpiler.transpile(expr, (&transpile::CExpr::Literal(0), &transpile::CTypeName::new("void".to_string())), &mut Vec::new());
    }
    println!("{}", transpiler);
}
