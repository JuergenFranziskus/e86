use super::span::Span;
use logos::Logos;


pub fn lex<'a>(src: &'a str) -> Vec<Token<'a>> {
    TokenKind::lexer(src).spanned()
        .map(|(t, s)| {
            let span = Span::new(s.start, s.end - s.start);
            Token {
                span,
                kind: t,
            }
        })
        .collect()
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: TokenKind<'a>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[derive(Logos)]
pub enum TokenKind<'a> {
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,

    #[token("word")]
    Word,


    #[token("mov")]
    Mov,
    #[token("add")]
    Add,
    #[token("store")]
    Store,
    #[token("jmp")]
    Jump,

    #[token("equ")]
    Equ,

    #[regex(r"r[1-9][0-9]*|rip")]
    Register(&'a str),

    #[regex(r"[_a-zA-Z][0-9a-zA-Z_]*")]
    Identifier(&'a str),

    #[regex(r"-?[0-9][_0-9]*")]
    Decimal(&'a str),

    #[regex(r"-?0[xX][0-9a-fA-F]+")]
    Hex(&'a str),

    #[regex(r"\n")]
    Newline,

    #[error]
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r"#[.^\n]*\n", logos::skip)]
    Error,
}


