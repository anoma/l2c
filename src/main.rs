mod sexpr;
mod expr;

fn main() {
    println!("Program: {:?}", expr::parse_program("{hello [world]}").unwrap());
}
