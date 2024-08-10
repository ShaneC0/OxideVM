mod layers;

use layers::scanner::{Scanner, TokenType};
use layers::vm::VM;
use layers::parser::{Parser, ParseError, Expr};
use layers::compiler::Compiler;

fn print_tokens(input: &str) {
    let mut scanner = Scanner::new(&input);
    loop {
        let t = scanner.next();
        if t.kind == TokenType::EOF {
            break;
        }
        println!("{:#?} {:?}", t.kind, t.lexeme);
    }
}

fn run_prog(input: &str) -> Result<Expr, Vec<ParseError>> {
    let mut p = Parser::new(&input);
    p.parse()
}
fn go(input: &str) {
    println!("Input String: {}", input);
    print_tokens(&input);
    let ast = run_prog(&input).unwrap();
    let mut c = Compiler::new();
    c.compile(ast);
    println!("Bytecode:");
    for inst in &c.chunk {
        print!("{:02x} ", inst);
    }
    println!("\nExec:");
    let mut vm = VM::new(c.chunk.clone(), c.constants.clone());
    vm.run();
}

fn main() {
    let input = String::from("1 + ( 2 * 3 }");
    go(&input);
}
