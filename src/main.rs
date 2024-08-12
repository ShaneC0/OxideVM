mod layers;
use layers::parser::Parser;
use layers::compiler::Compiler;
use layers::vm::VM;

fn pipeline(input: &str) {
    let mut p = Parser::new(input);
    match p.parse() {
        Ok(prog) => {
            println!("{}", prog);
            let mut c = Compiler::new();
            c.compile(prog);
            println!("");
            let mut vm = VM::new(c.chunk.clone(), c.constants.clone(), c.interner.clone());
            vm.run();
        }
        Err(errors) => println!("{:?}", errors),
    }
}

fn main() {
    let input = "let x = 2841 * 125 / 16243 + (1285 + 82 - (1295125)) + (109241 * 018295125) / 1254 + 10825;
                 let y = 10825 * 1012085 / 10825212;
                 print x + y;
                 print x > y;
                 let z = x + y;
                 print z - x;";
    pipeline(input);
}
