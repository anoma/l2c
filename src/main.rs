mod sexpr;
mod expr;
mod transpile;
mod expand;
use std::collections::HashMap;
use std::rc::Rc;
use crate::expr::Expr;
use std::collections::HashSet;

fn collect_free_variables(expr: &Expr, bound: &HashSet<String>, free: &mut HashSet<String>) {
    match expr {
        Expr::Symbol(sym) if !bound.contains(&sym.reference) => {
            free.insert(sym.reference.clone());
        },
        Expr::Symbol(_) | Expr::Literal(_) => {},
        Expr::If(ife) => {
            collect_free_variables(&ife.condition, bound, free);
            collect_free_variables(&ife.consequent, bound, free);
            collect_free_variables(&ife.alternate, bound, free);
        },
        Expr::With(with) => {
            let mut bound = bound.clone();
            bound.insert(with.reference.clone());
            collect_free_variables(&with.expression, &bound, free);
        },
        Expr::Continuation(continuation) => {
            let mut bound = bound.clone();
            bound.insert(continuation.reference.clone());
            for param in &continuation.parameters {
                bound.insert(param.clone());
            }
            collect_free_variables(&continuation.expression, &bound, free);
        },
        Expr::Jump(jump) => {
            collect_free_variables(&jump.reference, bound, free);
            for arg in &jump.arguments {
                collect_free_variables(arg, bound, free);
            }
        },
        Expr::Function(function) => {
            let mut bound = HashSet::new();
            bound.insert(function.reference.clone());
            for param in &function.parameters {
                bound.insert(param.clone());
            }
            collect_free_variables(&function.expression, &bound, free);
        },
        Expr::Invoke(invoke) => {
            collect_free_variables(&invoke.reference, bound, free);
            for arg in &invoke.arguments {
                collect_free_variables(arg, bound, free);
            }
        },
        Expr::Storage(storage) => {
            let mut bound = bound.clone();
            bound.insert(storage.reference.clone());
            for arg in &storage.arguments {
                collect_free_variables(&arg, &bound, free);
            }
        },
        Expr::Meta(_meta) => panic!("meta expressions should have been expanded already"),
    }
}

fn rename_variable(from: &mut String, mapping: &mut HashMap<String, String>, used: &mut HashSet<String>) {
    let base = from.replace("-", "_");
    let base = from.replace(".", "_");
    let mut reference = base.clone();
    for i in 0.. {
        if used.contains(&reference) {
            reference = format!("{}{}", base, i);
        } else {
            used.insert(reference.clone());
            mapping.insert(from.clone(), reference.clone());
            *from = reference.clone();
            return;
        }
    }
}

fn rename_variables(expr: &mut Expr, mapping: &HashMap<String, String>, used: &mut HashSet<String>) {
    match expr {
        Expr::Literal(_) => {},
        Expr::Symbol(sym) => {
            if let Some(target) = mapping.get(&sym.reference) {
                sym.reference = target.clone();
            }
        },
        Expr::If(ife) => {
            rename_variables(&mut ife.condition, mapping, used);
            rename_variables(&mut ife.consequent, mapping, used);
            rename_variables(&mut ife.alternate, mapping, used);
        },
        Expr::With(with) => {
            let mut mapping = mapping.clone();
            rename_variable(&mut with.reference, &mut mapping, used);
            rename_variables(&mut with.expression, &mapping, used);
        },
        Expr::Continuation(continuation) => {
            let mut mapping = mapping.clone();
            rename_variable(&mut continuation.reference, &mut mapping, used);
            for param in &mut continuation.parameters {
                rename_variable(param, &mut mapping, used);
            }
            rename_variables(&mut continuation.expression, &mapping, used);
        },
        Expr::Jump(jump) => {
            rename_variables(&mut jump.reference, mapping, used);
            for arg in &mut jump.arguments {
                rename_variables(arg, mapping, used);
            }
        },
        Expr::Function(function) => {
            let mut mapping = HashMap::new();
            rename_variable(&mut function.reference, &mut mapping, used);
            for param in &mut function.parameters {
                rename_variable(param, &mut mapping, used);
            }
            rename_variables(&mut function.expression, &mapping, used);
        },
        Expr::Invoke(invoke) => {
            rename_variables(&mut invoke.reference, mapping, used);
            for arg in &mut invoke.arguments {
                rename_variables(arg, mapping, used);
            }
        },
        Expr::Storage(storage) => {
            let mut mapping = mapping.clone();
            rename_variable(&mut storage.reference, &mut mapping, used);
            for arg in &mut storage.arguments {
                rename_variables(arg, &mut mapping, used);
            }
        },
        Expr::Meta(_meta) => panic!("meta expressions should have been expanded already"),
    }
}

fn escape_analysis(expr: &mut Expr, escapes: bool, escapees: &mut HashSet<String>) {
    match expr {
        Expr::Literal(_) => {},
        Expr::If(ife) => {
            escape_analysis(&mut ife.condition, true, escapees);
            escape_analysis(&mut ife.consequent, true, escapees);
            escape_analysis(&mut ife.alternate, true, escapees);
        },
        Expr::Storage(storage) => {
            for arg in &mut storage.arguments {
                let mut expr_escapees = HashSet::new();
                escape_analysis(arg, true, &mut expr_escapees);
                expr_escapees.remove(&storage.reference);
                escapees.extend(expr_escapees);
            }
        },
        Expr::Invoke(invoke) => {
            escape_analysis(&mut invoke.reference, true, escapees);
            for arg in &mut invoke.arguments {
                escape_analysis(arg, true, escapees);
            }
        },
        Expr::Function(function) => {
            escape_analysis(&mut function.expression, true, &mut HashSet::new());
        },
        Expr::With(with) => {
            let mut expr_escapees = HashSet::new();
            escape_analysis(&mut with.expression, true, &mut expr_escapees);
            with.escapes |= expr_escapees.remove(&with.reference);
            escapees.extend(expr_escapees);
        },
        Expr::Continuation(continuation) => {
            continuation.escapes |= escapes;
            let mut expr_escapees = HashSet::new();
            escape_analysis(&mut continuation.expression, true, &mut expr_escapees);
            continuation.escapes |= expr_escapees.remove(&continuation.reference);
            for param in &continuation.parameters {
                expr_escapees.remove(param);
            }
            escapees.extend(expr_escapees);
        },
        Expr::Jump(jump) => {
            escape_analysis(&mut jump.reference, false, escapees);
            for arg in &mut jump.arguments {
                escape_analysis(arg, true, escapees);
            }
        },
        Expr::Symbol(symbol) if escapes => {
            escapees.insert(symbol.reference.clone());
        },
        Expr::Symbol(_) => {},
        Expr::Meta(_meta) => panic!("meta expressions should have been expanded already"),
    }
}

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
    for mut expr in program {
        escape_analysis(&mut expr, true, &mut HashSet::new());
        let mut reserved_names = HashSet::new();
        reserved_names.insert("return".to_string());
        reserved_names.insert("break".to_string());
        reserved_names.insert("if".to_string());
        reserved_names.insert("goto".to_string());
        reserved_names.insert("void".to_string());
        reserved_names.insert("for".to_string());
        reserved_names.insert("int".to_string());
        collect_free_variables(&expr, &HashSet::new(), &mut reserved_names);
        rename_variables(&mut expr, &HashMap::new(), &mut reserved_names);
        transpiler.transpile(expr, (&transpile::CExpr::Literal(0), &transpile::CTypeName::new("void".to_string())), &mut Vec::new(), &HashSet::new());
    }
    println!("{}", transpiler);
}
