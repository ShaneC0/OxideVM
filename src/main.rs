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
    let input = "let x = \"shane\"; { x = 20; print x; } print x;";
    pipeline(input);
}
