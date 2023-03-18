use super::span::Span;

#[derive(Clone, Debug)]
pub struct Ast<'a> {
    pub lines: Vec<Line<'a>>,
}
#[derive(Clone, Debug)]
pub struct Line<'a> {
    pub span: Span,
    pub label: Option<Label<'a>>,
    pub kind: LineKind<'a>,
}
#[derive(Clone, Debug)]
pub enum LineKind<'a> {
    Empty,
    Equ(Expr<'a>),
    Operation(Mnemonic, Vec<Arg<'a>>),
    DefineBytes(Vec<DBArg<'a>>),
}
impl LineKind<'_> {
    pub fn is_empty(&self) -> bool {
        matches!(self, LineKind::Empty)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Mnemonic {
    Mov,
    Load,
    Store,
    Jmp,
    JmpF,
    Add,
    Sub,
    Mul,
    TestNB,
    TestB,
    TestEQ,
    Call,
    Ret,
}

#[derive(Clone, Debug)]
pub struct Arg<'a> {
    pub span: Span,
    pub kind: ArgKind<'a>,
}
#[derive(Clone, Debug)]
pub enum ArgKind<'a> {
    Expr(Expr<'a>),
    Register(Register),
    Memory(MemArg<'a>),
}

#[derive(Copy, Clone, Debug)]
pub struct Register(pub u8);
#[derive(Clone, Debug)]
pub struct MemArg<'a> {
    pub span: Span,
    pub base: Option<Register>,
    pub index: Option<Register>,
    pub scale: Option<MemScale>,
    pub offset: Option<Expr<'a>>,
    pub size: Option<MemSize>,
}
#[derive(Copy, Clone, Debug)]
pub enum MemScale {
    One,
    Two,
    Four,
    Eight,
}
impl MemScale {
    pub fn instruction_bits(self) -> u8 {
        match self {
            Self::One => 0,
            Self::Two => 1,
            Self::Four => 2,
            Self::Eight => 3,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum MemSize {
    Byte,
    Short,
    Long,
    Word,
}
impl MemSize {
    pub fn instruction_bits(self) -> u8 {
        match self {
            Self::Byte => 0,
            Self::Short => 1,
            Self::Long => 2,
            Self::Word => 3,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DBArg<'a> {
    pub span: Span,
    pub kind: DBArgKind<'a>,
}

#[derive(Clone, Debug)]
pub enum DBArgKind<'a> {
    StringLiteral(&'a str),
    Expr(Expr<'a>),
}

#[derive(Clone, Debug)]
pub struct Expr<'a> {
    pub span: Span,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Debug)]
pub enum ExprKind<'a> {
    Identifier(Label<'a>),
    Decimal(&'a str),
    Hex(&'a str),
    Here,
    Paren(Box<Expr<'a>>),
    Binary(BinaryExpr, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryExpr {
    Add,
    Sub,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label<'a> {
    pub global: Option<&'a str>,
    pub local: Option<&'a str>,
}
impl<'a> Label<'a> {
    pub fn make_global(mut self, global: &'a str) -> Self {
        if self.global.is_none() {
            self.global = Some(global);
        }
        self
    }
}
