pub mod sexpr;
pub mod expr;
pub mod transpile;
pub mod expand;
use std::rc::Rc;
use std::collections::HashMap;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let source = std::fs::read_to_string(&args[1])
        .expect("unable to read source");
    let mut program = expr::parse_program(&source).unwrap();
    println!("Source Program:");
    for expr in &program {
        println!("{}", expr);
    }
    println!();
    println!("Expanded Program:");
    let mut global_env = HashMap::new();
    expand::init_sexprs(&mut global_env);
    for expr in &mut program {
        if let expr::Expr::Function(function) = expr {
            global_env.insert(function.reference.clone(), expr.clone());
        }
    }
    for expr in &mut program {
        println!("{}", expand::expand(expr, &global_env));
        if let expr::Expr::Function(function) = expr {
            global_env.insert(function.reference.clone(), expr.clone());
        }
    }
    println!();
    println!("Evaluated Program:");
    for (idx, expr) in program.iter_mut().enumerate() {
        println!("Evaluation {}: {}", idx, expand::eval(expr, 0, &HashMap::new(), &global_env, Rc::new(expand::identity)).unwrap());
    }
    println!();
    println!("Compiled Outputs:");
    let mut transpiler = transpile::Transpiler::default();
    for expr in program {
        transpiler.transpile(expr);
    }
    println!("{}", transpiler);
}
