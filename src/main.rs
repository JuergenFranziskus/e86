use e86::assembler::token::lex;



fn main() {
    let src = std::fs::read_to_string("programs/main.e86").unwrap();
    let tokens = lex(&src);
    for token in tokens {
        println!("{token:?}");
    }
}
