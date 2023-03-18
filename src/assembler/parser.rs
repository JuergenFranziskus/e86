use super::{
    ast::{
        Arg, ArgKind, Ast, BinaryExpr, DBArg, DBArgKind, Expr, ExprKind, Label, Line, MemArg,
        MemSize, Mnemonic, Register,
    },
    span::Span,
    token::{Token, TokenKind},
};
use crate::assembler::ast::LineKind;

pub struct Parser<'a, 'b> {
    tokens: &'b [Token<'a>],
    index: usize,
}
impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'b [Token<'a>]) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn parse(mut self) -> Ast<'a> {
        let mut lines = Vec::new();
        while !self.at_end() {
            lines.push(self.parse_line());
        }
        Ast { lines }
    }

    fn parse_line(&mut self) -> Line<'a> {
        let start = self.curr_span();
        let label = self.parse_line_label();
        let mut kind = LineKind::Empty;

        if !self.is_token(TokenKind::Newline) {
            kind = self.parse_line_kind();
        }

        if !self.at_end() {
            self.consume_token(TokenKind::Newline);
        }

        Line {
            span: start,
            label,
            kind,
        }
    }
    fn parse_line_label(&mut self) -> Option<Label<'a>> {
        if !matches!(self.curr().kind, TokenKind::Identifier(_)) {
            return None;
        }
        let label = self.parse_label();

        if self.is_token(TokenKind::Colon) {
            self.next();
        }

        Some(label)
    }

    fn parse_line_kind(&mut self) -> LineKind<'a> {
        if self.is_token(TokenKind::Equ) {
            self.parse_equ()
        } else if self.is_token(TokenKind::DefineBytes) {
            self.parse_db()
        } else {
            self.parse_instruction()
        }
    }
    fn parse_instruction(&mut self) -> LineKind<'a> {
        let mnemonic = self.parse_menmonic();
        let mut args = Vec::new();

        while !self.at_end() && !self.is_token(TokenKind::Newline) {
            args.push(self.parse_arg());
            if self.is_token(TokenKind::Comma) {
                self.next();
            } else {
                break;
            }
        }

        LineKind::Operation(mnemonic, args)
    }
    fn parse_equ(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::Equ);
        let value = self.parse_leaf_expr();
        LineKind::Equ(value)
    }
    fn parse_db(&mut self) -> LineKind<'a> {
        self.consume_token(TokenKind::DefineBytes);

        let mut args = Vec::new();
        while !self.at_end() && !self.is_token(TokenKind::Newline) {
            args.push(self.parse_dbarg());
            if self.is_token(TokenKind::Comma) {
                self.next();
            } else {
                break;
            }
        }

        LineKind::DefineBytes(args)
    }
    fn parse_dbarg(&mut self) -> DBArg<'a> {
        let start = self.curr_span();
        let mut end = start;
        let kind = match self.curr().kind {
            TokenKind::StringLiteral(lit) => {
                self.next();
                let start = 1;
                let end = lit.len() - 1;
                let lit = &lit[start..end];
                DBArgKind::StringLiteral(lit)
            }
            _ => {
                let val = self.parse_leaf_expr();
                end = val.span;
                DBArgKind::Expr(val)
            }
        };

        let span = Span::merge(start, end);
        DBArg { span, kind }
    }

    fn parse_menmonic(&mut self) -> Mnemonic {
        let mnemonic = match self.curr().kind {
            TokenKind::Add => Mnemonic::Add,
            TokenKind::Sub => Mnemonic::Sub,
            TokenKind::Jump => Mnemonic::Jmp,
            TokenKind::JumpF => Mnemonic::JmpF,
            TokenKind::Mov => Mnemonic::Mov,
            TokenKind::Load => Mnemonic::Load,
            TokenKind::Store => Mnemonic::Store,
            TokenKind::TestNB => Mnemonic::TestNB,
            TokenKind::TestB => Mnemonic::TestB,
            TokenKind::TestEQ => Mnemonic::TestEQ,
            TokenKind::Call => Mnemonic::Call,
            TokenKind::Ret => Mnemonic::Ret,
            TokenKind::Mul => Mnemonic::Mul,
            a => panic!("{a:?} is not a valid mnemonic"),
        };
        self.next();
        mnemonic
    }

    fn parse_arg(&mut self) -> Arg<'a> {
        use TokenKind::*;
        match self.curr().kind {
            OpenBracket | Word | Byte => self.parse_memory_arg(),
            Register(_) => self.parse_register_arg(),
            _ => self.parse_expr_arg(),
        }
    }

    fn parse_memory_arg(&mut self) -> Arg<'a> {
        let start = self.curr_span();
        let size = self.parse_memory_size();

        self.consume_token(TokenKind::OpenBracket);

        let base = self.parse_base_register();
        let offset = self.parse_offset(base.is_some());

        let end = self.curr_span();
        self.consume_token(TokenKind::CloseBracket);

        let span = Span::merge(start, end);
        Arg {
            span,
            kind: ArgKind::Memory(MemArg {
                span,
                base,
                index: None,
                scale: None,
                offset,
                size,
            }),
        }
    }
    fn parse_memory_size(&mut self) -> Option<MemSize> {
        let kind = match self.curr().kind {
            TokenKind::Word => MemSize::Word,
            TokenKind::Byte => MemSize::Byte,
            _ => return None,
        };
        self.next();
        Some(kind)
    }
    fn parse_base_register(&mut self) -> Option<Register> {
        if matches!(self.curr().kind, TokenKind::Register(_)) {
            Some(self.parse_register())
        } else {
            None
        }
    }
    fn parse_offset(&mut self, needs_plus: bool) -> Option<Expr<'a>> {
        if self.is_token(TokenKind::CloseBracket) {
            return None;
        }

        if needs_plus {
            self.consume_token(TokenKind::Plus);
        }

        Some(self.parse_leaf_expr())
    }

    fn parse_register_arg(&mut self) -> Arg<'a> {
        let start = self.curr_span();
        let reg = self.parse_register();

        Arg {
            span: start,
            kind: ArgKind::Register(reg),
        }
    }

    fn parse_expr_arg(&mut self) -> Arg<'a> {
        let expr = self.parse_leaf_expr();
        Arg {
            span: expr.span,
            kind: ArgKind::Expr(expr),
        }
    }

    fn parse_expr(&mut self) -> Expr<'a> {
        self.parse_bin_expr(i16::MIN)
    }
    fn parse_bin_expr(&mut self, min_bp: i16) -> Expr<'a> {
        let mut left = self.parse_leaf_expr();

        loop {
            let op = match self.curr().kind {
                TokenKind::Plus => BinaryExpr::Add,
                TokenKind::Minus => BinaryExpr::Sub,
                _ => break,
            };

            let (l_bp, r_bp) = binary_bp(op);
            if l_bp < min_bp {
                break;
            }
            self.next();

            let right = self.parse_bin_expr(r_bp);
            let span = Span::merge(left.span, right.span);
            left = Expr {
                span,
                kind: ExprKind::Binary(op, left.into(), right.into()),
            };
        }

        left
    }
    fn parse_leaf_expr(&mut self) -> Expr<'a> {
        let span = self.curr_span();
        let kind = match self.curr().kind {
            TokenKind::Identifier(_) => ExprKind::Identifier(self.parse_label()),
            TokenKind::Decimal(val) => {
                self.next();
                ExprKind::Decimal(val)
            }
            TokenKind::Hex(val) => {
                self.next();
                ExprKind::Hex(val)
            }
            TokenKind::Here => {
                self.next();
                ExprKind::Here
            }
            TokenKind::OpenParen => return self.parse_paren_expr(),
            a => panic!("Expected expression, found {a:?}"),
        };

        Expr { span, kind }
    }
    fn parse_paren_expr(&mut self) -> Expr<'a> {
        let start = self.curr_span();
        self.consume_token(TokenKind::OpenParen);
        let val = self.parse_expr();
        let end = self.curr_span();
        self.consume_token(TokenKind::CloseParen);

        Expr {
            span: Span::merge(start, end),
            kind: ExprKind::Paren(val.into()),
        }
    }

    fn parse_label(&mut self) -> Label<'a> {
        let TokenKind::Identifier(name) = self.curr().kind else { panic!() };
        self.next();

        let parts: Vec<_> = name.split('.').collect();

        if parts.len() == 1 {
            return Label {
                global: Some(parts[0]),
                local: None,
            };
        }

        let global = if parts[0] == "" { None } else { Some(parts[0]) };
        let local = if parts[1] == "" { None } else { Some(parts[1]) };

        Label { global, local }
    }
    fn parse_register(&mut self) -> Register {
        let TokenKind::Register(name) = self.curr().kind else { panic!() };

        let reg = match name {
            "r0" => Register(0),
            "r1" => Register(1),
            "r2" => Register(2),
            "r3" => Register(3),
            "r4" => Register(4),
            "r5" => Register(5),
            "r6" => Register(6),
            "r7" => Register(7),
            "r8" => Register(8),
            "r9" => Register(9),
            "r10" => Register(10),
            "r11" => Register(11),
            "r12" => Register(12),
            "r13" => Register(13),
            "r14" => Register(14),
            "r15" => Register(15),
            "r16" => Register(16),
            "r17" => Register(17),
            "r18" => Register(18),
            "r19" => Register(19),
            "r20" => Register(20),
            "r21" => Register(21),
            "r22" => Register(22),
            "r23" => Register(23),
            "r24" => Register(24),
            "r25" => Register(25),
            "r26" => Register(26),
            "r27" => Register(27),
            "r28" => Register(28),
            "r29" => Register(29),
            "r30" => Register(30),
            "rip" | "r31" => Register(31),
            _ => panic!(),
        };
        self.next();
        reg
    }
    fn at_end(&self) -> bool {
        self.index >= self.tokens.len()
    }
    fn next(&mut self) {
        self.index += 1;
    }
    fn consume_token(&mut self, kind: TokenKind) {
        assert!(
            self.is_token(kind),
            "Exepcted {kind:?}, found {:?}",
            self.try_curr()
        );
        self.next();
    }
    fn is_token(&self, kind: TokenKind) -> bool {
        self.try_curr().map(|t| t.kind) == Some(kind)
    }
    fn curr_span(&self) -> Span {
        self.curr().span
    }
    fn try_curr(&self) -> Option<Token<'a>> {
        self.tokens.get(self.index).copied()
    }
    fn curr(&self) -> Token<'a> {
        self.tokens[self.index]
    }
}

fn binary_bp(op: BinaryExpr) -> (i16, i16) {
    use BinaryExpr::*;
    match op {
        Add | Sub => (0, 1),
    }
}
