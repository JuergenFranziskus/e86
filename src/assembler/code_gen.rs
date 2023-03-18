use super::{
    ast::{
        Arg, ArgKind, Ast, BinaryExpr, DBArg, DBArgKind, Expr, ExprKind, Label, Line, LineKind,
        Mnemonic, Register,
    },
    relocations::RelocationMode,
};
use crate::assembler::ast::{MemScale, MemSize};
use num::{BigInt, Num, ToPrimitive};
use std::{collections::HashMap, str::FromStr};

const ALU_ADD: u8 = 0o20;
const ALU_SUB: u8 = 0o21;
const ALU_MUL: u8 = 0o22;
const ALU_SELECT: u8 = 0o24;
const ALU_TESTB: u8 = 0o43;
const ALU_TESTNB: u8 = 0o47;
const ALU_TESTEQ: u8 = 0o50;

const SHORT_ALU_ADD: u8 = 6;
const SHORT_ALU_MUL: u8 = 7;

const FLAGS_NONE: u8 = 0;
const FLAGS_WRITE: u8 = 1;
const FLAGS_READ: u8 = 2;

pub struct CodeGen<'a> {
    relocations: Vec<Relocation<'a>>,
    bytes: Vec<u8>,
    last_global: Option<&'a str>,
    labels: HashMap<Label<'a>, BigInt>,
}
impl<'a> CodeGen<'a> {
    pub fn new() -> Self {
        Self {
            relocations: Vec::new(),
            bytes: Vec::new(),
            last_global: None,
            labels: HashMap::new(),
        }
    }

    pub fn gen_code(mut self, program: &Ast<'a>) -> Vec<u8> {
        for line in &program.lines {
            self.gen_line(line);
        }
        self.apply_relocations();

        self.bytes
    }
    fn apply_relocations(&mut self) {
        for reloc in &self.relocations {
            let ctx = reloc.ctx;
            let value = self.eval_expr(&reloc.value, ctx).unwrap();
            let value = value.to_i64().unwrap();
            let address = reloc.ctx.address as usize;

            let value = match reloc.mode {
                RelocationMode::WordLea => encode_word_lea_immediate(value),
                RelocationMode::WordALU => encode_word_alu_immediate(value),
                RelocationMode::WordMemory => encode_word_mem_immediate(value),
            };

            for (i, byte) in value.to_le_bytes().into_iter().enumerate() {
                self.bytes[address + i] |= byte;
            }
        }
    }

    fn gen_line(&mut self, line: &Line<'a>) {
        let label = line.label.map(|l| self.process_line_label(l));

        match &line.kind {
            LineKind::Empty => (),
            &LineKind::Equ(ref val) => self.gen_equ(label, val),
            &LineKind::DefineBytes(ref args) => self.gen_db(args),
            &LineKind::Operation(mnemonic, ref args) => self.gen_operation(mnemonic, args),
        }
    }
    fn process_line_label(&mut self, label: Label<'a>) -> Label<'a> {
        let global = label.global;
        let local = label.local;

        if let Some(global) = global {
            assert!(local.is_none());
            self.last_global = Some(global);
        }

        let label = Self::make_global(self.last_global, label);
        self.labels.insert(label, self.address().into());
        label
    }
    fn gen_equ(&mut self, label: Option<Label<'a>>, value: &Expr) {
        let value = self.eval_expr(value, self.ctx()).unwrap();

        if let Some(label) = label {
            self.labels.insert(label, value);
        }
    }
    fn gen_db(&mut self, args: &[DBArg<'a>]) {
        for arg in args {
            let bytes = self.eval_dbarg(arg);
            self.bytes.extend(bytes);
        }
    }
    fn gen_operation(&mut self, mnemonic: Mnemonic, args: &[Arg<'a>]) {
        match mnemonic {
            Mnemonic::Add => self.gen_add(args),
            Mnemonic::Sub => self.gen_sub(args),
            Mnemonic::Mul => self.gen_mul(args),
            Mnemonic::Jmp => self.gen_jmp(args),
            Mnemonic::JmpF => self.gen_jmpf(args),
            Mnemonic::Mov => self.gen_mov(args),
            Mnemonic::Store => self.gen_mem(true, args),
            Mnemonic::Load => self.gen_mem(false, args),
            Mnemonic::TestNB => self.gen_test(args, ALU_TESTNB),
            Mnemonic::TestB => self.gen_test(args, ALU_TESTB),
            Mnemonic::TestEQ => self.gen_test(args, ALU_TESTEQ),
            Mnemonic::Call => self.gen_call(args),
            Mnemonic::Ret => self.gen_reg_reg_mov(Register(31), Register(30)),
        }
    }

    fn gen_add(&mut self, args: &[Arg<'a>]) {
        match args.len() {
            2 => self.gen_ip_add(args),
            3 => self.gen_oop_alu_op(args, ALU_ADD, FLAGS_NONE),
            _ => panic!(),
        }
    }
    fn gen_ip_add(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(dst) = args[0].kind else { panic!() };

        if let ArgKind::Register(src) = args[1].kind {
            self.gen_ip_reg_add(dst, src);
        } else {
            let ArgKind::Expr(e) = &args[1].kind else { panic!() };
            self.gen_ip_alu_op(SHORT_ALU_ADD, dst, e);
        }
    }
    fn gen_ip_reg_add(&mut self, dst: Register, src: Register) {
        let regs = encode_short_registers(dst, src);
        let op = 0b10_1010 << 2;

        let operation = op | regs;
        let bytes = operation.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_sub(&mut self, args: &[Arg<'a>]) {
        match args.len() {
            2 => self.gen_ip_sub(args),
            3 => self.gen_oop_alu_op(args, ALU_SUB, FLAGS_NONE),
            _ => panic!(),
        }
    }
    fn gen_ip_sub(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(dst) = args[0].kind else { panic!() };

        if let ArgKind::Register(src) = args[1].kind {
            self.gen_ip_reg_sub(dst, src);
        } else {
            let ArgKind::Expr(e) = &args[1].kind else { panic!() };
            self.gen_oop_imm_alu_op(false, dst, dst, e, ALU_SUB, FLAGS_NONE);
        }
    }
    fn gen_ip_reg_sub(&mut self, dst: Register, src: Register) {
        let regs = encode_short_registers(dst, src);
        let op = 0b10_1011 << 2;

        let operation = op | regs;
        let bytes = operation.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_mul(&mut self, args: &[Arg<'a>]) {
        match args.len() {
            2 => self.gen_ip_mul(args),
            3 => self.gen_oop_alu_op(args, ALU_MUL, FLAGS_NONE),
            _ => panic!(),
        }
    }
    fn gen_ip_mul(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(dst) = args[0].kind else { panic!() };

        if let ArgKind::Register(src) = args[1].kind {
            self.gen_ip_reg_mul(dst, src);
        } else {
            let ArgKind::Expr(e) = &args[1].kind else { panic!() };
            self.gen_ip_alu_op(SHORT_ALU_MUL, dst, e);
        }
    }
    fn gen_ip_reg_mul(&mut self, dst: Register, src: Register) {
        let regs = encode_short_registers(dst, src);
        let op = 0b10_1100 << 2;

        let operation = op | regs;
        let bytes = operation.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_ip_alu_op(&mut self, op: u8, dst: Register, imm: &Expr<'a>) {
        if let Some(val) = self.eval_expr(imm, self.ctx()) {
            let val = val.to_i64().unwrap();
            if val >= -16 && val < 15 {
                let prefix = 0b10 << 6;
                let regs = encode_short_registers(dst, Register(val.to_le_bytes()[0]));
                let op = (op as u16) << 2;
                let instruction = prefix | regs | op;
                let bytes = instruction.to_le_bytes();
                self.bytes.extend(bytes);
            } else {
                self.gen_oop_imm_alu_op(false, dst, dst, imm, short_to_long_alu(op), FLAGS_NONE);
            }
        } else {
            self.gen_oop_imm_alu_op(false, dst, dst, imm, short_to_long_alu(op), FLAGS_NONE);
        }
    }
    fn gen_oop_alu_op(&mut self, args: &[Arg<'a>], op: u8, flags_mode: u8) {
        let ArgKind::Register(dst) = args[0].kind else { panic!() };

        if let ArgKind::Expr(e) = &args[1].kind {
            let ArgKind::Register(src1) = args[2].kind else { panic!() };
            self.gen_oop_imm_alu_op(true, dst, src1, e, op, flags_mode)
        } else if let ArgKind::Expr(e) = &args[2].kind {
            let ArgKind::Register(src0) = args[1].kind else { panic!() };
            self.gen_oop_imm_alu_op(false, dst, src0, e, op, flags_mode)
        } else {
            let ArgKind::Register(src0) = args[1].kind else { panic!() };
            let ArgKind::Register(src1) = args[2].kind else { panic!() };

            self.gen_oop_all_reg_alu_op(dst, src0, src1, op, flags_mode);
        }
    }
    fn gen_oop_all_reg_alu_op(
        &mut self,
        dst: Register,
        src0: Register,
        src1: Register,
        op: u8,
        flags_mode: u8,
    ) {
        debug_assert!(op < 0b1000000);
        debug_assert!(flags_mode < 0b100);

        let flags_mode = (flags_mode as u32) << 14;
        let dst = dst.0 as u32;
        let src0 = (src0.0 as u32) << 21;
        let src1 = (src1.0 as u32) << 16;
        let alu_op = (op as u32) << 8;
        let prefix = 0b110u32 << 5;

        let op = flags_mode | dst | src0 | src1 | alu_op | prefix;
        let bytes = op.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_oop_imm_alu_op(
        &mut self,
        swap: bool,
        dst: Register,
        a: Register,
        imm: &Expr<'a>,
        op: u8,
        flags_mode: u8,
    ) {
        if let Some(val) = self.eval_expr(imm, self.ctx()) {
            let val = val.to_i64().unwrap();
            if val >= -256 && val < 255 {
                self.gen_long_oop_imm_alu_op(swap, dst, a, val as i32, op, flags_mode);
            } else {
                self.gen_word_oop_imm_alu_op(swap, dst, a, val, op, flags_mode);
            }
        } else {
            self.word_alu_relocation(imm.clone());
            self.gen_word_oop_imm_alu_op(swap, dst, a, 0, op, flags_mode);
        }
    }
    fn gen_long_oop_imm_alu_op(
        &mut self,
        swap: bool,
        dst: Register,
        a: Register,
        imm: i32,
        op: u8,
        flags_mode: u8,
    ) {
        let opcode = if swap { 3 } else { 2 };

        let imm = encode_long_alu_immediate(imm);
        let prefix = 0b110 << 5;
        let op = (op as u32) << 8;
        let mode = (flags_mode as u32) << 14;
        let regs = encode_long_registers(dst, a, Register(0));
        let opcode = (opcode as u32) << 30;

        let instruction = prefix | opcode | op | mode | regs | opcode | imm;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_word_oop_imm_alu_op(
        &mut self,
        swap: bool,
        dst: Register,
        a: Register,
        imm: i64,
        op: u8,
        flags_mode: u8,
    ) {
        let opcode = if swap { 2 } else { 1 };
        let imm = encode_word_alu_immediate(imm);
        let regs = encode_word_registers(dst, a, Register(0));
        let prefix = 0b1110 << 4;
        let op = (op as u64) << 24;
        let flags = (flags_mode as u64) << 30;

        let instruction = prefix | opcode | imm | regs | op | flags;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_test(&mut self, args: &[Arg<'a>], alu_op: u8) {
        match args.len() {
            2 => self.gen_binary_test(args, alu_op),
            3 => self.gen_oop_alu_op(args, alu_op, FLAGS_WRITE),
            _ => panic!(),
        }
    }
    fn gen_binary_test(&mut self, args: &[Arg<'a>], alu_op: u8) {
        use ArgKind as AK;
        match (&args[0].kind, &args[1].kind) {
            (&AK::Register(a), &AK::Register(b)) => {
                self.gen_oop_all_reg_alu_op(Register(0), a, b, alu_op, FLAGS_WRITE)
            }
            (&AK::Register(a), AK::Expr(b)) => {
                self.gen_oop_imm_alu_op(false, Register(0), a, b, alu_op, FLAGS_WRITE)
            }
            (AK::Expr(a), &AK::Register(b)) => {
                self.gen_oop_imm_alu_op(true, Register(0), b, a, alu_op, FLAGS_WRITE)
            }
            _ => panic!(),
        }
    }

    fn gen_jmp(&mut self, args: &[Arg<'a>]) {
        if let ArgKind::Expr(e) = &args[0].kind {
            self.gen_reg_imm_mov(Register(31), e);
        } else {
            todo!()
        }
    }
    fn gen_jmpf(&mut self, args: &[Arg<'a>]) {
        if let ArgKind::Expr(e) = &args[0].kind {
            self.gen_oop_imm_alu_op(false, Register(31), Register(31), e, ALU_SELECT, FLAGS_READ);
        } else {
            todo!()
        }
    }

    fn gen_mov(&mut self, args: &[Arg<'a>]) {
        let ArgKind::Register(dst) = args[0].kind else { panic!() };

        if let ArgKind::Register(src) = args[1].kind {
            self.gen_reg_reg_mov(dst, src);
        } else {
            let ArgKind::Expr(e) = &args[1].kind else { panic!() };
            self.gen_reg_imm_mov(dst, e);
        }
    }
    fn gen_reg_reg_mov(&mut self, dst: Register, src: Register) {
        if src.0 == 0 {
            self.gen_zero_reg(dst);
            return;
        }
        if dst.0 == 0 {
            return;
        }

        let regs = encode_short_registers(dst, src);
        let prefix = 0b10u16 << 6;

        let operation = prefix | regs;
        let bytes = operation.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_reg_imm_mov(&mut self, dst: Register, imm: &Expr<'a>) {
        match self.eval_expr(imm, self.ctx()) {
            None => {
                self.word_lea_relocation(imm.clone());
                self.gen_word_lea(dst, Register(0), Register(0), MemScale::One, 0);
            }
            Some(value) => {
                let value = value.to_i64().unwrap();
                if value == 0 {
                    self.gen_zero_reg(dst);
                } else if value >= -16 && value < 15 {
                    self.gen_short_mov_constant(dst, value as i8);
                } else {
                    self.gen_word_lea(dst, Register(0), Register(0), MemScale::One, value);
                }
            }
        }
    }
    fn gen_short_mov_constant(&mut self, dst: Register, imm: i8) {
        debug_assert!(imm >= -16);
        debug_assert!(imm < 15);

        let regs = encode_short_registers(dst, Register(imm.to_le_bytes()[0]));
        let op = 0b10_1101 << 2;

        let instruction = op | regs;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_zero_reg(&mut self, dst: Register) {
        let dst = dst.0;
        let bytes = dst.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_mem(&mut self, store: bool, args: &[Arg<'a>]) {
        let ArgKind::Register(dst) = args[0].kind else { panic!() };
        let ArgKind::Memory(mem) = &args[1].kind else { panic!() };

        let base = mem.base.unwrap_or(Register(0));
        let index = mem.index.unwrap_or(Register(0));
        let scale = mem.scale.unwrap_or(MemScale::One);
        let size = mem.size.unwrap_or(MemSize::Word);

        if let Some(offset) = &mem.offset {
            if let Some(val) = self.eval_expr(offset, self.ctx()) {
                let val = val.to_i64().unwrap();
                if val >= -64 && val < 63 {
                    self.gen_long_mem(store, dst, base, index, scale, size, val as i32)
                } else {
                    self.gen_word_mem(store, dst, base, index, scale, size, val);
                }
            } else {
                self.word_mem_relocation(offset.clone());
                self.gen_word_mem(store, dst, base, index, scale, size, 0);
            }
        } else {
            self.gen_long_mem(store, dst, base, index, scale, size, 0);
        }
    }
    fn gen_long_mem(
        &mut self,
        store: bool,
        dst: Register,
        base: Register,
        index: Register,
        scale: MemScale,
        size: MemSize,
        offset: i32,
    ) {
        let regs = encode_long_registers(dst, base, index);
        let size = (size.instruction_bits() as u32) << 12;
        let scale = (scale.instruction_bits() as u32) << 14;

        let prefix = 0b1100_0000;
        let op = if store { 2 } else { 1 } << 29;
        let offset = encode_long_mem_immediate(offset);

        let instruction = size | scale | regs | prefix | op | offset;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }
    fn gen_word_mem(
        &mut self,
        store: bool,
        dst: Register,
        base: Register,
        index: Register,
        scale: MemScale,
        size: MemSize,
        offset: i64,
    ) {
        let regs = encode_word_registers(dst, base, index);
        let op = if store { 0b1110_0100 } else { 0b1110_0011 };
        let scale = (scale.instruction_bits() as u64) << 26;
        let size = (size.instruction_bits() as u64) << 24;
        let offset = encode_word_mem_immediate(offset);

        let instruction = size | scale | regs | op | offset;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_word_lea(
        &mut self,
        dst: Register,
        base: Register,
        index: Register,
        scale: MemScale,
        offset: i64,
    ) {
        let offset = encode_word_lea_immediate(offset);

        let dst = (dst.0 as u64) << 19;
        let base = (base.0 as u64) << 14;
        let index = (index.0 as u64) << 9;
        let scale = (scale.instruction_bits() as u64) << 26;

        let op = 0b1110_0101;

        let instruction = op | dst | base | index | scale | offset;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn gen_call(&mut self, args: &[Arg<'a>]) {
        let dst;
        let mem;
        if args.len() == 1 {
            dst = Register(30);
            let ArgKind::Memory(m) = &args[0].kind else { panic!() };
            mem = m;
        } else if args.len() == 2 {
            let ArgKind::Register(d) = args[0].kind else { panic!() };
            let ArgKind::Memory(m) = &args[1].kind else { panic!() };
            dst = d;
            mem = m;
        } else {
            panic!();
        }

        let base = mem.base.unwrap_or(Register(0));
        let index = mem.index.unwrap_or(Register(0));
        let scale = mem.scale.unwrap_or(MemScale::One);

        if let Some(offset) = &mem.offset {
            if let Some(val) = self.eval_expr(offset, self.ctx()) {
                let val = val.to_i64().unwrap();
                self.gen_call_prime(dst, base, index, scale, val as i64)
            } else {
                self.word_lea_relocation(offset.clone());
                self.gen_call_prime(dst, base, index, scale, 0);
            }
        } else {
            self.gen_call_prime(dst, base, index, scale, 0);
        }
    }
    fn gen_call_prime(
        &mut self,
        dst: Register,
        base: Register,
        index: Register,
        scale: MemScale,
        offset: i64,
    ) {
        let offset = encode_word_lea_immediate(offset);

        let dst = (dst.0 as u64) << 19;
        let base = (base.0 as u64) << 14;
        let index = (index.0 as u64) << 9;
        let scale = (scale.instruction_bits() as u64) << 26;

        let op = 0b1110_0110;

        let instruction = op | dst | base | index | scale | offset;
        let bytes = instruction.to_le_bytes();
        self.bytes.extend(bytes);
    }

    fn eval_expr(&self, e: &Expr<'a>, ctx: ExprCtx<'a>) -> Option<BigInt> {
        match &e.kind {
            &ExprKind::Decimal(num) => Some(BigInt::from_str(num).unwrap()),
            &ExprKind::Hex(num) => Some(BigInt::from_str_radix(&num[2..], 16).unwrap()),
            &ExprKind::Identifier(name) => {
                let full_label = Self::make_global(ctx.global_label, name);
                let val = self.labels.get(&full_label).cloned();
                val
            }
            &ExprKind::Here => Some(ctx.address.into()),
            &ExprKind::Paren(ref e) => self.eval_expr(e, ctx),
            &ExprKind::Binary(op, ref a, ref b) => {
                let a = self.eval_expr(a, ctx)?;
                let b = self.eval_expr(b, ctx)?;
                let result = match op {
                    BinaryExpr::Add => a + b,
                    BinaryExpr::Sub => a - b,
                };
                Some(result)
            }
        }
    }
    fn eval_dbarg(&self, arg: &DBArg<'a>) -> Vec<u8> {
        let ctx = self.ctx();
        match &arg.kind {
            DBArgKind::Expr(e) => self.eval_expr(e, ctx).unwrap().to_signed_bytes_le(),
            DBArgKind::StringLiteral(s) => s.as_bytes().to_vec(),
        }
    }

    fn address(&self) -> u64 {
        self.bytes.len() as u64
    }
    fn ctx(&self) -> ExprCtx<'a> {
        ExprCtx {
            address: self.address(),
            global_label: self.last_global,
        }
    }

    fn word_alu_relocation(&mut self, value: Expr<'a>) {
        let ctx = self.ctx();
        self.relocations.push(Relocation {
            mode: RelocationMode::WordALU,
            ctx,
            value,
        });
    }
    fn word_mem_relocation(&mut self, value: Expr<'a>) {
        let ctx = self.ctx();
        self.relocations.push(Relocation {
            mode: RelocationMode::WordMemory,
            ctx,
            value,
        });
    }
    fn word_lea_relocation(&mut self, value: Expr<'a>) {
        let ctx = self.ctx();
        self.relocations.push(Relocation {
            mode: RelocationMode::WordLea,
            ctx,
            value,
        });
    }

    fn make_global(global: Option<&'a str>, label: Label<'a>) -> Label<'a> {
        match (global, label.global) {
            (_, Some(_)) => label,
            (Some(a), None) => label.make_global(a),
            (None, None) => panic!("Cannot make {label:?} global with {global:?}"),
        }
    }
}

fn short_to_long_alu(op: u8) -> u8 {
    match op {
        SHORT_ALU_ADD => ALU_ADD,
        SHORT_ALU_MUL => ALU_MUL,
        _ => panic!(),
    }
}

fn encode_short_registers(a: Register, b: Register) -> u16 {
    let dst_low = (a.0 as u16 & 0b11) << 0;
    let dst_high = (a.0 as u16 & 0b11100) << 11;
    let src = (b.0 as u16) << 8;

    src | dst_high | dst_low
}
fn encode_long_registers(d: Register, a: Register, b: Register) -> u32 {
    let d = d.0 as u32;
    let a = (a.0 as u32) << 21;
    let b = (b.0 as u32) << 16;

    d | a | b
}
fn encode_word_registers(d: Register, a: Register, b: Register) -> u64 {
    let d = (d.0 as u64) << 19;
    let a = (a.0 as u64) << 14;
    let b = (b.0 as u64) << 9;
    d | a | b
}
fn encode_long_alu_immediate(value: i32) -> u32 {
    let value = u32::from_ne_bytes(value.to_ne_bytes());
    let low = (value & 0b11111) << 16;
    let high = (value & !0b11111) << 26 - 5;

    low | high
}
fn encode_word_alu_immediate(value: i64) -> u64 {
    let value = u64::from_ne_bytes(value.to_ne_bytes());
    let low = (value & 0b111111) << 8;
    let high = (value & !0b111111) << 32 - 6;

    low | high
}
fn encode_word_lea_immediate(offset: i64) -> u64 {
    let offset = u64::from_ne_bytes(offset.to_ne_bytes());
    let offset0 = (offset & 1) << 8;
    let offset1 = (offset & 0b110) << 24 - 1;
    let offset2 = (offset & !0b111) << 28 - 3;

    offset0 | offset1 | offset2
}
fn encode_long_mem_immediate(offset: i32) -> u32 {
    let offset = u32::from_ne_bytes(offset.to_ne_bytes());
    let low = (offset & 0b1111) << 8;
    let high = (offset & !0b1111) << 26 - 4;

    low | high
}
fn encode_word_mem_immediate(offset: i64) -> u64 {
    let offset = u64::from_ne_bytes(offset.to_ne_bytes());
    let offset0 = (offset & 1) << 8;
    let offset1 = (offset & !1) << 28 - 1;

    offset0 | offset1
}

struct Relocation<'a> {
    mode: RelocationMode,
    ctx: ExprCtx<'a>,
    value: Expr<'a>,
}

#[derive(Copy, Clone, Debug)]
struct ExprCtx<'a> {
    global_label: Option<&'a str>,
    address: u64,
}
