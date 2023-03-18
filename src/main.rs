use e86::assembler::{code_gen::CodeGen, parser::Parser, token::lex};

fn main() {
    let src = std::fs::read_to_string("programs/main.e86").unwrap();
    let tokens = lex(&src);

    let ast = Parser::new(&tokens).parse();
    let bytes = CodeGen::new().gen_code(&ast);

    println!("v3.0 hex words addressed");
    for (i, chunk) in bytes.chunks(16).enumerate() {
        print_hex(i * 16);
        print!(": ");
        for &byte in chunk {
            print_hex(byte as usize);
            print!(" ");
        }
        println!();
    }
}

fn print_hex(num: usize) {
    let num = &format!("{num:#04x}")[2..];
    print!("{num}");
}
