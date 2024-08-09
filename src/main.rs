mod layers;

use layers::scanner::{Scanner, TokenType};
use layers::vm::{VM, OpCode};
use layers::parser::{Parser};



fn main() {
    let input = String::from("1 + 2 / 5 * 3");
    let mut p = Parser::new(&input);
    p.parse();
}
