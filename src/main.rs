mod sexpr;
mod expr;
use expr::{Expr, InvokeExpr, LiteralExpr, SymbolExpr, IfExpr, FunctionExpr, JumpExpr, WithExpr, ContinuationExpr, StorageExpr};

#[derive(Clone)]
pub enum CExpr {
    Symbol(String),
    Literal(u32),
    Call(Box<CExpr>, Vec<CExpr>),
    AddressOf(Box<CExpr>),
    Not(Box<CExpr>),
    Subscript(Box<CExpr>, Box<CExpr>),
}

impl std::fmt::Display for CExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Symbol(string) => write!(f, "{}", string),
            Self::Literal(value) => write!(f, "{}", value),
            Self::Call(reference, args) => {
                write!(f, "{}(", reference)?;
                if let [arg0, rest @ ..] = &args[..] {
                    write!(f, "{}", arg0)?;
                    for arg in rest {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, ")")
            },
            Self::AddressOf(expr) => write!(f, "&{}", expr),
            Self::Not(expr) => write!(f, "!{}", expr),
            Self::Subscript(expr1, expr2) => write!(f, "{}[{}]", expr1, expr2),
        }
    }
}

pub enum CStmt {
    Assign(CExpr, CExpr),
    If(CExpr, Vec<CStmt>, Vec<CStmt>),
    Return(CExpr),
    Function(String, Vec<String>, Vec<CStmt>),
    Comment(String),
}

impl std::fmt::Display for CStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Assign(dst, src) => writeln!(f, "{} = {};", dst, src),
            Self::If(cond, cons, alt) => {
                writeln!(f, "if({}) {{", cond)?;
                for stmt in cons {
                    write!(f, "{}", stmt)?;
                }
                write!(f, "}}")?;
                if alt.is_empty() {
                    writeln!(f)
                } else {
                    writeln!(f, " else {{")?;
                    for stmt in alt {
                        write!(f, "{}", stmt)?;
                    }
                    writeln!(f, "}}")
                }
            },
            Self::Return(val) => writeln!(f, "return {};", val),
            Self::Function(reference, params, body) => {
                write!(f, "uintptr_t {}(", reference)?;
                if let [param0, rest @ ..] = &params[..] {
                    write!(f, "uintptr_t {}", param0)?;
                    for param in rest {
                        write!(f, ", uintptr_t {}", param)?;
                    }
                }
                writeln!(f, ") {{")?;
                for stmt in body {
                    write!(f, "{}", stmt)?;
                }
                writeln!(f, "}}")
            },
            Self::Comment(comment) => writeln!(f, "// {}", comment),
        }
    }
}

#[derive(Default)]
pub struct State {
    counter: u32,
    jmp_vars: Vec<String>,
}

impl State {
    pub fn gen_sym(&mut self) -> String {
        let sym = format!("tmp{}", self.counter);
        self.counter += 1;
        sym
    }

    pub fn jmp_var(&mut self, idx: usize) -> &String {
        for _ in self.jmp_vars.len()..(idx+1) {
            let new_sym = self.gen_sym();
            self.jmp_vars.push(new_sym);
        }
        &self.jmp_vars[idx]
    }
}

pub fn transpile_expr(expr: Expr, target: &CExpr, local: &mut Vec<CStmt>, global: &mut Vec<CStmt>, state: &mut State) {
    let comment = CStmt::Comment(expr.to_string());
    match expr {
        Expr::Symbol(SymbolExpr { reference }) => {
            let assignment = CStmt::Assign(target.clone(), CExpr::Symbol(reference));
            local.push(assignment);
        },
        Expr::Literal(LiteralExpr { value }) => {
            let assignment = CStmt::Assign(target.clone(), CExpr::Literal(value));
            local.push(assignment);
        },
        Expr::With(WithExpr { reference, expression }) => {
            local.push(comment);
            let mut withbody = vec![];
            let jmp_var = CExpr::Symbol(state.jmp_var(0).clone());
            let cret = CExpr::Symbol(state.gen_sym());
            transpile_expr(*expression, &cret, &mut withbody, global, state);
            withbody.push(CStmt::Assign(jmp_var.clone(), cret));
            let call = CExpr::Call(Box::new(CExpr::Symbol("setjmp".to_string())), vec![CExpr::Symbol(reference)]);
            let cif = CStmt::If(CExpr::Not(Box::new(call)), withbody, vec![]);
            local.push(cif);
            local.push(CStmt::Assign(target.clone(), jmp_var));
        },
        Expr::Function(FunctionExpr { reference, parameters, expression }) => {
            global.push(comment);
            let mut funbody = vec![];
            let cret = CExpr::Symbol(state.gen_sym());
            transpile_expr(*expression, &cret, &mut funbody, global, state);
            funbody.push(CStmt::Return(cret));
            let function = CStmt::Function(reference.clone(), parameters, funbody);
            global.push(function);
            local.push(CStmt::Assign(target.clone(), CExpr::AddressOf(Box::new(CExpr::Symbol(reference)))));
        },
        Expr::Continuation(ContinuationExpr { reference, parameters, expression }) => {
            local.push(comment);
            let mut contbody = vec![];
            let cret = CExpr::Symbol(state.gen_sym());
            for (i, param) in parameters.into_iter().enumerate() {
                contbody.push(CStmt::Assign(CExpr::Symbol(param), CExpr::Symbol(state.jmp_var(i).to_string())));
            }
            transpile_expr(*expression, &cret, &mut contbody, global, state);
            let call = CExpr::Call(Box::new(CExpr::Symbol("setjmp".to_string())), vec![CExpr::Symbol(reference.clone())]);
            let cif = CStmt::If(call, contbody, vec![]);
            local.push(cif);
            local.push(CStmt::Assign(target.clone(), CExpr::Symbol(reference)));
        },
        Expr::Invoke(InvokeExpr { reference, arguments }) => {
            local.push(comment);
            let cref = CExpr::Symbol(state.gen_sym());
            transpile_expr(*reference, &cref, local, global, state);
            let mut cargs = vec![];
            for arg in arguments {
                let carg = CExpr::Symbol(state.gen_sym());
                transpile_expr(arg, &carg, local, global, state);
                cargs.push(carg);
            }
            let call = CExpr::Call(Box::new(cref), cargs);
            local.push(CStmt::Assign(target.clone(), call));
        },
        Expr::Jump(JumpExpr { reference, arguments }) => {
            local.push(comment);
            let cref = CExpr::Symbol(state.gen_sym());
            transpile_expr(*reference, &cref, local, global, state);
            let mut cargs = vec![];
            for arg in arguments {
                let carg = CExpr::Symbol(state.gen_sym());
                transpile_expr(arg, &carg, local, global, state);
                cargs.push(carg);
            }
            for (i, carg) in cargs.into_iter().enumerate() {
                local.push(CStmt::Assign(CExpr::Symbol(state.jmp_var(i).clone()), carg));
            }
            let call = CExpr::Call(Box::new(CExpr::Symbol("longjmp".to_string())), vec![cref, CExpr::Literal(1)]);
            local.push(CStmt::Assign(target.clone(), call));
        },
        Expr::If(IfExpr { condition, consequent, alternate }) => {
            local.push(comment);
            let ccond = CExpr::Symbol(state.gen_sym());
            transpile_expr(*condition, &ccond, local, global, state);
            let mut cconsequent = vec![];
            transpile_expr(*consequent, &target, &mut cconsequent, global, state);
            let mut calternate = vec![];
            transpile_expr(*alternate, &target, &mut calternate, global, state);
            let ifstmt = CStmt::If(ccond, cconsequent, calternate);
            local.push(ifstmt);
        },
        Expr::Storage(StorageExpr { reference, arguments }) => {
            local.push(comment);
            let cref = CExpr::Symbol(reference);
            for (i, arg) in arguments.into_iter().enumerate() {
                let i = i.try_into().expect("storage too large");
                let celt = CExpr::Subscript(Box::new(cref.clone()), Box::new(CExpr::Literal(i)));
                transpile_expr(arg, &celt, local, global, state);
            }
            local.push(CStmt::Assign(target.clone(), cref));
        },
        Expr::Meta(_) => panic!("meta expressions should already have been expanded")
    }
}

fn main() {
    let mut state = State::default();
    let mut local = Vec::new();
    let mut global = Vec::new();
    let program = expr::parse_program("(function hi (aa) (if a {hello [world]} [car]))").unwrap();
    println!("Source Program:");
    for expr in program {
        println!("{}", expr);
        transpile_expr(expr, &CExpr::Literal(0), &mut local, &mut global, &mut state);
    }
    println!();
    println!("Compiled Outputs:");
    for stmt in global {
        print!("{}", stmt);
    }
}
