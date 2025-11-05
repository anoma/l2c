use crate::expr::{Expr, LiteralExpr, JumpExpr, InvokeExpr, StorageExpr, ContinuationExpr, MetaExpr, parse_expr};
use crate::sexpr::SExpr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct JumpBuffer {
    target_id: u32,
    args: Vec<Expr>,
    counter: u32,
}

pub type Continuation<'a> = Rc<dyn Fn(&Expr, u32) -> Result<Expr, JumpBuffer> + 'a>;

pub fn identity(expr: &Expr, _counter: u32) -> Result<Expr, JumpBuffer> {
    Ok(expr.clone())
}

pub fn init_sexprs(env: &mut HashMap<String, Expr>) {
    let nil = Expr::Meta(MetaExpr { reference: None, fragment: SExpr::List(vec![]) });
    env.insert("nil".to_string(), nil);
    for i in 33u8..127u8 {
        let reference = vec!['-', i as char, '-'].into_iter().collect();
        let sexpr = Expr::Meta(MetaExpr { reference: None, fragment: SExpr::Char(i as char) });
        env.insert(reference, sexpr);
    }
}

fn eval_invoke(invoke: &InvokeExpr, counter: u32, env: &HashMap<String, Expr>, global_env: &HashMap<String, Expr>, args: Vec<Expr>) -> Result<Expr, JumpBuffer> {
    let i = args.len();
    if i < invoke.arguments.len() {
        eval(&invoke.arguments[i], counter, env, global_env, Rc::new(|arg, counter| {
            let mut args = args.clone();
            args.push(arg.clone());
            eval_invoke(invoke, counter, env, global_env, args)
        }))
    } else {
        eval(&invoke.reference, counter, env, global_env, Rc::new(|reference, counter| {
            match (reference, &args[..]) {
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "+" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value.wrapping_add(b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "-" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value.wrapping_sub(b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "*" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value.wrapping_mul(b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "/" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value.wrapping_div(b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "%" =>
                    Ok(Expr::Literal(LiteralExpr { value: a.value.wrapping_rem(b.value) })),
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
                    Ok(Expr::Literal(LiteralExpr { value: (a.value << b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == ">>" =>
                    Ok(Expr::Literal(LiteralExpr { value: (a.value >> b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "&" =>
                    Ok(Expr::Literal(LiteralExpr { value: (a.value & b.value) })),
                (Expr::Symbol(sym), [Expr::Literal(a), Expr::Literal(b)]) if sym.reference == "|" =>
                    Ok(Expr::Literal(LiteralExpr { value: (a.value | b.value) })),
                (Expr::Symbol(sym), [Expr::Meta(MetaExpr { fragment: SExpr::List(list), reference })]) if sym.reference == "cdr" =>
                    Ok(Expr::Meta(MetaExpr { fragment: SExpr::List(list[1..].to_vec()), reference: reference.clone() })),
                (Expr::Symbol(sym), [Expr::Meta(MetaExpr { fragment: SExpr::List(list), reference })]) if sym.reference == "car" =>
                    Ok(Expr::Meta(MetaExpr { fragment: list[0].clone(), reference: reference.clone() })),
                (Expr::Symbol(sym), [Expr::Meta(MetaExpr { fragment, .. }), Expr::Meta(MetaExpr { fragment: SExpr::List(list), .. })]) if sym.reference == "cons" => {
                    let mut list = list.clone();
                    list.insert(0, fragment.clone());
                    Ok(Expr::Meta(MetaExpr { fragment: SExpr::List(list), reference: None }))
                },
                (Expr::Symbol(sym), [Expr::Meta(MetaExpr { fragment: SExpr::List(list), .. })]) if sym.reference == "null?" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(list.is_empty()) })),
                (Expr::Symbol(sym), [Expr::Meta(MetaExpr { fragment: SExpr::Char(a), .. }), Expr::Meta(MetaExpr { fragment: SExpr::Char(b), .. })]) if sym.reference == "char=" =>
                    Ok(Expr::Literal(LiteralExpr { value: u32::from(a == b) })),
                (Expr::Symbol(sym), [Expr::Meta(_), Expr::Meta(_)]) if sym.reference == "char=" => {
                    Ok(Expr::Literal(LiteralExpr { value: 0u32 }))},
                (Expr::Symbol(sym), [Expr::Meta(MetaExpr { fragment: SExpr::List(list), .. })]) if sym.reference == "token?" => {
                    for elt in list {
                        let SExpr::Char(_) = elt else {
                            return Ok(Expr::Literal(LiteralExpr { value: 0u32 }));
                        };
                    }
                    Ok(Expr::Literal(LiteralExpr { value: 1u32 }))
                },
                (Expr::Symbol(sym), [_]) if sym.reference == "token?" =>
                    Ok(Expr::Literal(LiteralExpr { value: 0u32 })),
                (Expr::Function(function), _) if function.parameters.len() == args.len() => {
                    let mut env = HashMap::new();
                    env.insert(function.reference.clone(), reference.clone());
                    for (param, arg) in function.parameters.iter().zip(args.clone().into_iter()) {
                        env.insert(param.clone(), arg);
                    }
                    eval(&function.expression, counter, &env, global_env, Rc::new(identity))
                },
                (Expr::Function(_), _) => panic!("incorrect argument count to: {reference}"),
                _ => panic!("unknown function: {:?}", Expr::Invoke(InvokeExpr {reference: Box::new(reference.clone()), arguments: args.clone()})),
            }
        }))
    }
}

fn eval_jump(jump: &JumpExpr, counter: u32, env: &HashMap<String, Expr>, global_env: &HashMap<String, Expr>, args: &[Expr]) -> Result<Expr, JumpBuffer> {
    let i = args.len();
    if i < jump.arguments.len() {
        eval(&jump.arguments[i], counter, env, global_env, Rc::new(|arg: &Expr, counter| {
            let mut args = args.to_owned();
            args.push(arg.clone());
            eval_jump(jump, counter, env, global_env, &args)
        }))
    } else {
        eval(&jump.reference, counter, env, global_env, Rc::new(|reference, counter| {
            let Expr::Literal(reference) = reference else {
                panic!("jump reference has not been reduced to a literal")
            };
            Err(JumpBuffer { target_id: reference.value, args: args.to_owned(), counter })
        }))
    }
}

fn eval_storage(storage: &StorageExpr, counter: u32, env: &HashMap<String, Expr>, global_env: &HashMap<String, Expr>, cont: Continuation, args: Vec<Expr>) -> Result<Expr, JumpBuffer> {
    let i = args.len();
    if i < storage.arguments.len() {
        eval(&storage.arguments[i], counter, env, global_env, Rc::new(|arg, counter| {
            let mut args = args.clone();
            args.push(arg.clone());
            eval_storage(storage, counter, env, global_env, cont.clone(), args)
        }))
    } else {
        let mut storage = storage.clone();
        storage.arguments = args;
        (*cont)(&Expr::Storage(storage), counter)
    }
}

fn eval_continuation(continuation: &ContinuationExpr, continuation_id: u32, env: &HashMap<String, Expr>, global_env: &HashMap<String, Expr>, result: Result<Expr, JumpBuffer>) -> Result<Expr, JumpBuffer>  {
    let continuation_id_expr = Expr::Literal(LiteralExpr { value: continuation_id });
    match result {
        Err(JumpBuffer { target_id, args, counter }) if target_id == continuation_id && args.len() == continuation.parameters.len() => {
            let mut env = (*env).clone();
            env.insert(continuation.reference.clone(), continuation_id_expr.clone());
            for (param, arg) in continuation.parameters.iter().zip(args.into_iter()) {
                env.insert(param.clone(), arg);
            }
            let undefined_cont: Continuation = Rc::new(|_, _| panic!("evaluation of the continuation expression should never return"));
            let result = eval(&continuation.expression, counter, &env, global_env, undefined_cont);
            eval_continuation(continuation, continuation_id, &env, global_env, result)
        },
        Err(JumpBuffer { target_id, .. }) if target_id == continuation_id => {
            panic!("incorrect number of arguments to continuation")
        },
        value => value,
    }
}

pub fn eval(expr: &Expr, counter: u32, env: &HashMap<String, Expr>, global_env: &HashMap<String, Expr>, cont: Continuation) -> Result<Expr, JumpBuffer> {
    match expr {
        Expr::Symbol(sym) => cont(env.get(&sym.reference).or_else(|| global_env.get(&sym.reference)).unwrap_or(expr), counter),
        Expr::If(ife) => {
            eval(&ife.condition, counter, env, global_env, Rc::new(|condition, counter| {
                match condition {
                    Expr::Literal(value) if value.value != 0 => {
                        eval(&ife.consequent, counter, env, global_env, cont.clone())
                    },
                    Expr::Literal(_) => {
                        eval(&ife.alternate, counter, env, global_env, cont.clone())
                    },
                    _ => panic!("if condition has not been reduced to a literal"),
                }
            }))
        },
        Expr::With(with) => {
            let mut env = (*env).clone();
            let continuation_id = counter;
            env.insert(with.reference.clone(), Expr::Literal(LiteralExpr { value: continuation_id }));
            match eval(&with.expression, counter + 1, &env, global_env, cont.clone()) {
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
            eval_jump(jump, counter, env, global_env, &[])
        },
        Expr::Continuation(continuation) => {
            let continuation_id = counter;
            let continuation_id_expr = Expr::Literal(LiteralExpr { value: continuation_id });
            eval_continuation(continuation, continuation_id, env, global_env, cont(&continuation_id_expr, counter + 1))
        },
        Expr::Invoke(invoke) => {
            cont(&eval_invoke(invoke, counter, env, global_env, vec![])?, counter)
        },
        Expr::Storage(storage) => {
            let mut env = (*env).clone();
            env.insert(storage.reference.clone(), expr.clone());
            eval_storage(storage, counter, &env, global_env, cont, vec![])
        },
        Expr::Meta(meta) if meta.reference.is_some() => {
            eval(expand(&mut expr.clone(), global_env), counter, env, global_env, cont)
        }
        Expr::Literal(_) | Expr::Function(_) | Expr::Meta(_) => cont(expr, counter),
    }
}

pub fn expand<'a>(expr: &'a mut Expr, env: &HashMap<String, Expr>) -> &'a mut Expr {
    match expr {
        Expr::If(ife) => {
            expand(&mut ife.condition, env);
            expand(&mut ife.consequent, env);
            expand(&mut ife.alternate, env);
        },
        Expr::With(with) => {
            expand(&mut with.expression, env);
        },
        Expr::Jump(jump) => {
            expand(&mut jump.reference, env);
            for arg in &mut jump.arguments {
                expand(arg, env);
            }
        },
        Expr::Continuation(continuation) => {
            expand(&mut continuation.expression, env);
        },
        Expr::Function(function) => {
            expand(&mut function.expression, env);
        },
        Expr::Invoke(invoke) => {
            expand(&mut invoke.reference, env);
            for arg in &mut invoke.arguments {
                expand(arg, env);
            }
        },
        Expr::Storage(storage) => {
            for arg in &mut storage.arguments {
                expand(arg, env);
            }
        },
        Expr::Meta(MetaExpr { reference: Some(reference), fragment }) => {
            let counter = 0;
            let reference = eval(reference, counter, &HashMap::new(), env, Rc::new(identity)).expect("jump must not escape macro expression");
            let arguments = vec![Expr::Meta(MetaExpr { reference: None, fragment: fragment.clone() })];
            let expansion_expr = Expr::Invoke(InvokeExpr { reference: Box::new(reference), arguments: arguments.clone() });
            let expansion = eval(&expansion_expr, counter, &HashMap::new(), env, Rc::new(identity)).expect("jump must not escape macro");
            let Expr::Meta(meta) = expansion else {
                panic!("macro did not return a valid s-expression");
            };
            *expr = parse_expr(meta.fragment).expect("unable to parse expanded macro");
            expand(expr, env);
        },
        Expr::Literal(_) | Expr::Symbol(_) | Expr::Meta(_)=> {},
    }
    expr
}
