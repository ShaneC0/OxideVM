mod layers;
use layers::compiler::Compiler;
use layers::parser::Parser;
use layers::vm::VM;

fn pipeline(input: &str) {
    let mut p = Parser::new(input);
    match p.parse() {
        Ok(prog) => {
            let mut c = Compiler::new();
            c.compile(prog);
            let mut vm = VM::new(c.chunk.clone(), c.constants.clone(), c.interner.clone());
            vm.run();
        }
        Err(errors) => println!("{:?}", errors),
    }
}

fn main() {
    let input = "
        let x = 0;
        let y = 1;
        while(y < 10201010) {
            let temp = y;
            y = x + y;
            x = temp;
            print y;
        }
    ";
    pipeline(input);
}
