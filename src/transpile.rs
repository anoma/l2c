use crate::expr::{Expr, InvokeExpr, LiteralExpr, SymbolExpr, IfExpr, FunctionExpr, JumpExpr, WithExpr, ContinuationExpr, StorageExpr};

#[derive(Clone)]
pub struct CTypeName(String, Box<CDeclarator>);

impl CTypeName {
    pub fn new(spec: String) -> Self {
        Self(spec, Box::new(CDeclarator::Identifier("".to_string())))
    }
}

#[derive(Clone)]
pub enum CExpr {
    Symbol(String),
    Literal(u32),
    Call(Box<CExpr>, Vec<CExpr>),
    Binary(String, Box<CExpr>, Box<CExpr>),
    AddressOf(Box<CExpr>),
    Not(Box<CExpr>),
    Subscript(Box<CExpr>, Box<CExpr>),
    Indirection(Box<CExpr>),
    Cast(CTypeName, Box<CExpr>),
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
            Self::Indirection(expr) => write!(f, "*{}", expr),
            Self::Not(expr) => write!(f, "!{}", expr),
            Self::Subscript(expr1, expr2) => write!(f, "{}[{}]", expr1, expr2),
            Self::Cast(typename, expr) => write!(f, "({} {}) {}", typename.0, typename.1, expr),
            Self::Binary(op, expr1, expr2) => write!(f, "({} {} {})", expr1, op, expr2),
        }
    }
}

#[derive(Clone)]
pub struct CFunctionDeclarator(Box<CDeclarator>, Vec<(String, CDeclarator)>);

impl std::fmt::Display for CFunctionDeclarator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let decl = &self.0;
        let params = &self.1;
        write!(f, "{}(", decl)?;
        if let [(spec0, decl0), rest @ ..] = &params[..] {
            write!(f, "{} {}", spec0, decl0)?;
            for (spec, decl) in rest {
                write!(f, ", {} {}", spec, decl)?;
            }
        }
        write!(f, ")")
    }
}

impl CFunctionDeclarator {
    pub fn identifier(self, ident: String) -> Self {
        Self(Box::new(self.0.identifier(ident)), self.1)
    }
}

#[derive(Clone)]
pub enum CDeclarator {
    Identifier(String),
    Pointer(Box<CDeclarator>),
    Array(Box<CDeclarator>, CExpr),
    Function(CFunctionDeclarator),
}

impl CDeclarator {
    pub fn identifier(self, ident: String) -> Self {
        match self {
            Self::Identifier(_) => Self::Identifier(ident),
            Self::Pointer(decl) => Self::Pointer(Box::new(decl.identifier(ident))),
            Self::Array(decl, expr) => Self::Array(Box::new(decl.identifier(ident)), expr),
            Self::Function(decl) => Self::Function(decl.identifier(ident)),
        }
    }
}

impl Default for CDeclarator {
    fn default() -> Self {
        Self::Identifier(String::new())
    }
}

impl std::fmt::Display for CDeclarator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{}", ident),
            Self::Pointer(decl) => write!(f, "(*{})", decl),
            Self::Array(decl, expr) => write!(f, "({}[{}])", decl, expr),
            Self::Function(decl) => write!(f, "{}", decl),
        }
    }
}

pub enum CStmt {
    Declaration(String, Vec<(CDeclarator, Option<CExpr>)>),
    Assign(CExpr, CExpr),
    If(CExpr, Vec<CStmt>, Vec<CStmt>),
    Return(CExpr),
    Function(String, CFunctionDeclarator, Vec<CStmt>),
    Comment(String),
    Expr(CExpr),
}

impl std::fmt::Display for CStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Declaration(spec, decls) => {
                write!(f, "{}", spec)?;
                if let [(decl0, init0), rest @ ..] = &decls[..] {
                    write!(f, " {}", decl0)?;
                    if let Some(init0) = init0 {
                        write!(f, " = {}", init0)?;
                    }
                    for (decl, init) in rest {
                        write!(f, ", {}", decl)?;
                        if let Some(init) = init {
                            write!(f, " = {}", init)?;
                        }
                    }
                }
                writeln!(f, ";")
            },
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
            Self::Function(spec, decl, body) => {
                writeln!(f, "{} {} {{", spec, decl)?;
                for stmt in body {
                    write!(f, "{}", stmt)?;
                }
                writeln!(f, "}}")
            },
            Self::Comment(comment) => writeln!(f, "// {}", comment),
            Self::Expr(expr) => writeln!(f, "{};", expr),
        }
    }
}

#[derive(Default)]
pub struct Transpiler {
    counter: u32,
    jmpargs: Vec<String>,
    cprogam: Vec<CStmt>,
}

impl Transpiler {
    pub fn gen_sym(&mut self) -> String {
        let sym = format!("tmp{}", self.counter);
        self.counter += 1;
        sym
    }

    pub fn jmp_var(&mut self, idx: usize) -> &String {
        for _ in self.jmpargs.len()..(idx+1) {
            let new_sym = self.gen_sym();
            self.jmpargs.push(new_sym.clone());
            self.cprogam.insert(0, CStmt::Declaration("uintptr_t".to_string(), vec![(CDeclarator::Identifier(new_sym), None)]));
        }
        &self.jmpargs[idx]
    }

    pub fn is_symbol(expr: &Expr, set: &[&str]) -> bool {
        let Expr::Symbol(sym) = expr else {
            return false;
        };
        set.contains(&sym.reference.as_str())
    }

    pub fn transpile(&mut self, expr: Expr, target: (&CExpr, &CTypeName), block: &mut Vec<CStmt>) {
        let comment = CStmt::Comment(expr.to_string());
        match expr {
            Expr::Symbol(SymbolExpr { reference }) => {
                let assignment = CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(CExpr::Symbol(reference))));
                block.push(assignment);
            },
            Expr::Literal(LiteralExpr { value }) => {
                let assignment = CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(CExpr::Literal(value))));
                block.push(assignment);
            },
            Expr::With(WithExpr { reference, expression }) => {
                block.push(comment);
                let cbuf_name = self.gen_sym();
                let cbuf = CExpr::Symbol(cbuf_name.clone());
                let cbuf_decl = (CDeclarator::Identifier(cbuf_name), None);
                let reference_decl = (CDeclarator::Pointer(Box::new(CDeclarator::Identifier(reference.clone()))), Some(CExpr::AddressOf(Box::new(cbuf.clone()))));
                block.push(CStmt::Declaration("jmp_buf".to_string(), vec![cbuf_decl, reference_decl]));
                let mut withbody = vec![];
                let jmp_var = CExpr::Symbol(self.jmp_var(0).clone());
                let cret_name = self.gen_sym();
                let cret_type = CTypeName::new("uintptr_t".to_string());
                withbody.push(CStmt::Declaration(cret_type.0.clone(), vec![(cret_type.1.clone().identifier(cret_name.clone()), None)]));
                let cret = CExpr::Symbol(cret_name);
                self.transpile(*expression, (&cret, &cret_type), &mut withbody);
                withbody.push(CStmt::Assign(jmp_var.clone(), cret));
                let call = CExpr::Call(Box::new(CExpr::Symbol("setjmp".to_string())), vec![cbuf]);
                let cif = CStmt::If(CExpr::Not(Box::new(call)), withbody, vec![]);
                block.push(cif);
                block.push(CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(jmp_var))));
            },
            Expr::Function(FunctionExpr { reference, parameters, expression }) => {
                let mut cparams = vec![];
                for param in &parameters {
                    cparams.push(("uintptr_t".to_string(), CDeclarator::Identifier(param.clone())));
                }
                let cfunc_decl = CFunctionDeclarator(Box::new(CDeclarator::Identifier(reference.clone())), cparams);
                let cfunc_type = CTypeName("uintptr_t".to_string(), Box::new(CDeclarator::Function(cfunc_decl.clone())));
                self.cprogam.insert(0, CStmt::Declaration(cfunc_type.0.to_string(), vec![(*cfunc_type.1, None)]));
                self.cprogam.push(comment);
                let mut funbody = vec![];
                let cret_name = self.gen_sym();
                let cret_type = CTypeName::new("uintptr_t".to_string());
                let cret = CExpr::Symbol(cret_name.clone());
                funbody.push(CStmt::Declaration(cret_type.0.clone(), vec![(cret_type.1.clone().identifier(cret_name), None)]));
                self.transpile(*expression, (&cret, &cret_type), &mut funbody);
                funbody.push(CStmt::Return(cret));
                let function = CStmt::Function(cfunc_type.0.clone(), cfunc_decl, funbody);
                self.cprogam.push(function);
                block.push(CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(CExpr::AddressOf(Box::new(CExpr::Symbol(reference)))))));
            },
            Expr::Continuation(ContinuationExpr { reference, parameters, expression }) => {
                block.push(comment);
                let cbuf_name = self.gen_sym();
                let cbuf = CExpr::Symbol(cbuf_name.clone());
                let cbuf_decl = (CDeclarator::Identifier(cbuf_name), None);
                let reference_decl = (CDeclarator::Pointer(Box::new(CDeclarator::Identifier(reference.clone()))), Some(CExpr::AddressOf(Box::new(cbuf.clone()))));
                block.push(CStmt::Declaration("jmp_buf".to_string(), vec![cbuf_decl, reference_decl]));
                let mut contbody = vec![];
                let cret_name = self.gen_sym();
                let cret_type = CTypeName::new("uintptr_t".to_string());
                let cret = CExpr::Symbol(cret_name.clone());
                contbody.push(CStmt::Declaration(cret_type.0.clone(), vec![(cret_type.1.clone().identifier(cret_name), None)]));
                for (i, param) in parameters.into_iter().enumerate() {
                    contbody.push(CStmt::Declaration("uintptr_t".to_string(), vec![(CDeclarator::Identifier(param), Some(CExpr::Symbol(self.jmp_var(i).to_string())))]));
                }
                self.transpile(*expression, (&cret, &cret_type), &mut contbody);
                let call = CExpr::Call(Box::new(CExpr::Symbol("setjmp".to_string())), vec![cbuf]);
                let cif = CStmt::If(call, contbody, vec![]);
                block.push(cif);
                block.push(CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(CExpr::Symbol(reference)))));
            },
            Expr::Invoke(InvokeExpr { reference, arguments }) if Self::is_symbol(&reference, &["+", "-", "/", "*", "<<", ">>", "==", "!=", "<", ">", "<=", ">="]) && arguments.len() == 2 => {
                block.push(comment);
                let Expr::Symbol(sym) = *reference else {
                    panic!("invocation reference should be a symbol");
                };
                let mut cargs = vec![];
                for arg in arguments {
                    let carg_name = self.gen_sym();
                    let carg_type = CTypeName::new("uintptr_t".to_string());
                    block.push(CStmt::Declaration(carg_type.0.to_string(), vec![(carg_type.1.clone().identifier(carg_name.clone()), None)]));
                    let carg = CExpr::Symbol(carg_name);
                    self.transpile(arg, (&carg, &carg_type), block);
                    cargs.push(carg);
                }
                let call = CExpr::Binary(sym.reference, Box::new(cargs[0].clone()), Box::new(cargs[1].clone()));
                block.push(CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(call))));
            },
            Expr::Invoke(InvokeExpr { reference, arguments }) => {
                block.push(comment);
                let cref_name = self.gen_sym();
                let cref = CExpr::Symbol(cref_name.clone());
                let mut params = vec![];
                for _ in &arguments {
                    params.push(("uintptr_t".to_string(), CDeclarator::default()));
                }
                let cref_type = CTypeName("uintptr_t".to_string(), Box::new(CDeclarator::Function(CFunctionDeclarator(Box::new(CDeclarator::Pointer(Box::new(CDeclarator::default()))), params.clone()))));
                block.push(CStmt::Declaration(cref_type.0.to_string(), vec![(cref_type.1.clone().identifier(cref_name), None)]));
                self.transpile(*reference, (&cref, &cref_type), block);
                let mut cargs = vec![];
                for arg in arguments {
                    let carg_name = self.gen_sym();
                    let carg_type = CTypeName::new("uintptr_t".to_string());
                    block.push(CStmt::Declaration(carg_type.0.to_string(), vec![(carg_type.1.clone().identifier(carg_name.clone()), None)]));
                    let carg = CExpr::Symbol(carg_name);
                    self.transpile(arg, (&carg, &carg_type), block);
                    cargs.push(carg);
                }
                let call = CExpr::Call(Box::new(cref), cargs);
                block.push(CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(call))));
            },
            Expr::Jump(JumpExpr { reference, arguments }) => {
                block.push(comment);
                let cref_name = self.gen_sym();
                let cref_type = CTypeName("jmp_buf".to_string(), Box::new(CDeclarator::Pointer(Box::new(CDeclarator::default()))));
                let cref = CExpr::Symbol(cref_name.clone());
                block.push(CStmt::Declaration(cref_type.0.clone(), vec![(cref_type.1.clone().identifier(cref_name), None)]));
                self.transpile(*reference, (&cref, &cref_type), block);
                let mut cargs = vec![];
                for arg in arguments {
                    let carg_name = self.gen_sym();
                    let carg_type = CTypeName::new("uintptr_t".to_string());
                    block.push(CStmt::Declaration(carg_type.0.clone(), vec![(carg_type.1.clone().identifier(carg_name.clone()), None)]));
                    let carg = CExpr::Symbol(carg_name);
                    self.transpile(arg, (&carg, &carg_type), block);
                    cargs.push(carg);
                }
                for (i, carg) in cargs.into_iter().enumerate() {
                    block.push(CStmt::Assign(CExpr::Symbol(self.jmp_var(i).clone()), carg));
                }
                let call = CExpr::Call(Box::new(CExpr::Symbol("longjmp".to_string())), vec![CExpr::Indirection(Box::new(cref)), CExpr::Literal(1)]);
                block.push(CStmt::Expr(call));
            },
            Expr::If(IfExpr { condition, consequent, alternate }) => {
                block.push(comment);
                let ccond_name = self.gen_sym();
                let ccond = CExpr::Symbol(ccond_name.clone());
                let ccond_type = CTypeName::new("uintptr_t".to_string());
                block.push(CStmt::Declaration(ccond_type.0.clone(), vec![(ccond_type.1.clone().identifier(ccond_name), None)]));
                self.transpile(*condition, (&ccond, &ccond_type), block);
                let mut cconsequent = vec![];
                self.transpile(*consequent, target, &mut cconsequent);
                let mut calternate = vec![];
                self.transpile(*alternate, target, &mut calternate);
                let ifstmt = CStmt::If(ccond, cconsequent, calternate);
                block.push(ifstmt);
            },
            Expr::Storage(StorageExpr { reference, arguments }) => {
                block.push(comment);
                let cref = CExpr::Symbol(reference.clone());
                let cref_type = CTypeName("uintptr_t".to_string(), Box::new(CDeclarator::Array(Box::new(CDeclarator::default()), CExpr::Literal(arguments.len().try_into().expect("storage too large")))));
                block.push(CStmt::Declaration(cref_type.0.clone(), vec![(cref_type.1.identifier(reference), None)]));
                for (i, arg) in arguments.into_iter().enumerate() {
                    let i = i.try_into().expect("storage too large");
                    let celt = CExpr::Subscript(Box::new(cref.clone()), Box::new(CExpr::Literal(i)));
                    let celt_type = CTypeName::new("uintptr_t".to_string());
                    self.transpile(arg, (&celt, &celt_type), block);
                }
                block.push(CStmt::Assign(target.0.clone(), CExpr::Cast(target.1.clone(), Box::new(cref))));
            },
            Expr::Meta(_) => panic!("meta expressions should already have been expanded")
        }
    }
}

impl std::fmt::Display for Transpiler {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        println!("#include <stdint.h>");
        println!("#include <setjmp.h>");
        println!();
        for stmt in &self.cprogam {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
