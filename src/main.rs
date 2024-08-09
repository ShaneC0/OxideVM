mod layers;

use layers::scanner::{Scanner, TokenType};
use layers::vm::{VM, OpCode};
use layers::parser::{Parser};



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

fn run_prog(input: &str) {
    let mut p = Parser::new(&input);
    let result = p.parse();
    match result {
        Ok(_) => println!("hooray"),
        Err(_) => println!("oh no!")
    }
}

fn main() {
    let input = String::from("-2 + -5.0 / (20) * 2.5400 - 500");
    run_prog(&input);
}
