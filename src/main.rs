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
            print!("Bytecode: ");
            for inst in &c.chunk {
                print!("{:02x} ", inst);
            }
            println!("\nConstants: {:?}", c.constants);
            println!("Interner: {:?}", c.interner);
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
        while(y < 1000000) {
            let temp = x;
            x = x + y;
            y = temp;
            print y;
        }
    ";
    pipeline(input);
}
