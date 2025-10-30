mod sexpr;
mod expr;
mod transpile;
use crate::expr::{Expr, LiteralExpr, JumpExpr, InvokeExpr, StorageExpr, ContinuationExpr, parse_expr};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
struct JumpBuffer {
    target_id: u32,
    args: Vec<Expr>,
    counter: u32,
}

type Continuation<'a> = Rc<dyn Fn(&Expr, u32) -> Result<Expr, JumpBuffer> + 'a>;

fn identity(expr: &Expr, _counter: u32) -> Result<Expr, JumpBuffer> {
    return Ok(expr.clone());
}

fn eval_invoke(invoke: &InvokeExpr, counter: u32, env: &HashMap<String, Expr>, args: Vec<Expr>) -> Result<Expr, JumpBuffer> {
    let i = args.len();
    if i < invoke.arguments.len() {
        eval(&invoke.arguments[i], counter, env, Rc::new(|arg, counter| {
            let mut args = args.clone();
            args.push(arg.clone());
            eval_invoke(invoke, counter, env, args)
        }))
    } else {
        eval(&invoke.reference, counter, env, Rc::new(|reference, counter| {
            match (reference, &args[..]) {
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "+" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value + b.value })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "-" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value - b.value })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "*" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value * b.value })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "/" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value / b.value })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "==" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value == b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "!=" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value != b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "<=" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value <= b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == ">=" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value >= b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "<" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value < b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == ">" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value > b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "<<" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value << b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == ">>" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a.value >> b.value) })),
                (Expr::Function(function), _) => {
                    let mut env = HashMap::new();
                    env.insert(function.reference.clone(), reference.clone());
                    for (param, arg) in function.parameters.iter().zip(args.clone().into_iter()) {
                        env.insert(param.clone(), arg);
                    }
                    eval(&function.expression, counter, &env, Rc::new(identity))
                },
                _ => panic!("unknown function"),
            }
        }))
    }
}

fn eval_jump(jump: &JumpExpr, counter: u32, env: &HashMap<String, Expr>, args: &Vec<Expr>) -> Result<Expr, JumpBuffer> {
    let i = args.len();
    if i < jump.arguments.len() {
        eval(&jump.arguments[i], counter, env, Rc::new(|arg: &Expr, counter| {
            let mut args = args.clone();
            args.push(arg.clone());
            eval_jump(jump, counter, env, &args)
        }))
    } else {
        eval(&jump.reference, counter, env, Rc::new(|reference, counter| {
            let Expr::Literal(reference) = reference else {
                panic!("jump reference has not been reduced to a literal")
            };
            return Err(JumpBuffer { target_id: reference.value, args: args.clone(), counter })
        }))
    }
}

fn eval_storage(storage: &StorageExpr, counter: u32, env: &HashMap<String, Expr>, cont: Continuation, args: Vec<Expr>) -> Result<Expr, JumpBuffer> {
    let i = args.len();
    if i < storage.arguments.len() {
        eval(&storage.arguments[i], counter, env, Rc::new(|arg, counter| {
            let mut args = args.clone();
            args.push(arg.clone());
            eval_storage(storage, counter, env, cont.clone(), args)
        }))
    } else {
        let mut storage = storage.clone();
        storage.arguments = args;
        (*cont)(&Expr::Storage(storage), counter)
    }
}

fn eval_continuation(continuation: &ContinuationExpr, continuation_id: u32, env: &HashMap<String, Expr>, result: Result<Expr, JumpBuffer>) -> Result<Expr, JumpBuffer>  {
    let continuation_id_expr = Expr::Literal(LiteralExpr { value: continuation_id });
    match result {
        Err(JumpBuffer { target_id, args, counter }) if target_id == continuation_id && args.len() == continuation.parameters.len() => {
            let mut env = (*env).clone();
            env.insert(continuation.reference.clone(), continuation_id_expr.clone());
            for (param, arg) in continuation.parameters.iter().zip(args.into_iter()) {
                env.insert(param.clone(), arg);
            }
            let undefined_cont: Continuation = Rc::new(|_, _| panic!("evaluation of the continuation expression should never return"));
            let result = eval(&continuation.expression, counter, &env, undefined_cont);
            eval_continuation(continuation, continuation_id, &env, result)
        },
        Err(JumpBuffer { target_id, .. }) if target_id == continuation_id => {
            panic!("incorrect number of arguments to continuation")
        },
        value => value,
    }
}

fn eval(expr: &Expr, counter: u32, env: &HashMap<String, Expr>, cont: Continuation) -> Result<Expr, JumpBuffer> {
    match expr {
        Expr::Literal(_) | Expr::Function(_) => cont(expr, counter),
        Expr::Symbol(sym) => cont(env.get(&sym.reference).unwrap_or(expr), counter),
        Expr::If(ife) => {
            eval(&ife.condition, counter, env, Rc::new(|condition, counter| {
                match condition {
                    Expr::Literal(value) if value.value != 0 => {
                        eval(&ife.consequent, counter, env, cont.clone())
                    },
                    Expr::Literal(_) => {
                        eval(&ife.alternate, counter, env, cont.clone())
                    },
                    _ => panic!("if condition has not been reduced to a literal"),
                }
            }))
        },
        Expr::With(with) => {
            let mut env = (*env).clone();
            let continuation_id = counter;
            env.insert(with.reference.clone(), Expr::Literal(LiteralExpr { value: continuation_id }));
            match eval(&with.expression, counter + 1, &env, cont.clone()) {
                Err(JumpBuffer { target_id, mut args, counter }) if target_id == continuation_id && args.len() == 1 => {
                    cont(&args.remove(0), counter)
                },
                Err(JumpBuffer { target_id, .. }) if target_id == continuation_id => {
                    panic!("incorrect number of arguments to with")
                },
                value => value,
            }
        },
        Expr::Jump(jump) => {
            eval_jump(jump, counter, env, &vec![])
        },
        Expr::Continuation(continuation) => {
            let continuation_id = counter;
            let continuation_id_expr = Expr::Literal(LiteralExpr { value: continuation_id });
            eval_continuation(continuation, continuation_id, env, cont(&continuation_id_expr, counter + 1))
        },
        Expr::Invoke(invoke) => {
            cont(&eval_invoke(invoke, counter, env, vec![])?, counter)
        },
        Expr::Storage(storage) => {
            let mut env = (*env).clone();
            env.insert(storage.reference.clone(), expr.clone());
            eval_storage(storage, counter, &env, cont, vec![])
        },
        Expr::Meta(meta) => {
            let reference = eval(&meta.reference, counter, &HashMap::new(), Rc::new(identity)).expect("jump must not escape macro expression");
            let expansion_expr = Expr::Invoke(InvokeExpr { reference: Box::new(reference), arguments: vec![expr.clone()] });
            let expansion = eval(&expansion_expr, counter, &HashMap::new(), Rc::new(identity)).expect("jump must not escape macro");
            let Expr::Meta(meta) = expansion else {
                panic!("macro did not return a valid s-expression");
            };
            let expansion = parse_expr(meta.fragment).expect("unable to parse expanded macro");
            eval(&expansion, counter, env, cont)
        }
    }
}

fn main() {
    //let program = expr::parse_program("(function hi (aa) [+ (with hello (if a {hello [world 55]} [car])) 56])").unwrap();
    //let program = expr::parse_program("(function hi (aa) (storage hey 1 2 9))").unwrap();
    let program = expr::parse_program("[(function factorial (n) (if [== n (literal 00000000000000000000000000000000)] (literal 00000000000000000000000000000001) [* n [factorial [- n (literal 00000000000000000000000000000001)]]])) (literal 00000000000000000000000000000101)]").unwrap();
    /*let program = expr::parse_program(
        "[(function factorial (n) (with ret {(continuation loop (m acc) (if [== m (literal 00000000000000000000000000000000)] {ret acc} {loop [- m (literal 00000000000000000000000000000001)] [* m acc]})) n (literal 00000000000000000000000000000001)})) (literal 00000000000000000000000000000011)]"
    ).unwrap();*/
    let mut transpiler = transpile::Transpiler::default();
    println!("Source Program:");
    for expr in program {
        println!("{}", expr);
        let result = eval(&expr, 0, &HashMap::new(), Rc::new(identity)).unwrap();
        transpiler.transpile(expr, (&transpile::CExpr::Literal(0), &transpile::CTypeName::new("void".to_string())), &mut Vec::new());
        println!("Result: {}", result);
    }
    println!();
    println!("Compiled Outputs:");
    println!("{}", transpiler);
}
