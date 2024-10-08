use std::{collections::HashMap, error::Error, io::Write, rc::Rc};

use crate::{
    ast::{ExprEnum, Expression, Span, Statement, Statements, TypeDecl},
    bytecode::{global_functions, ByteCode, FnByteCode, FnDecl, FnDef},
    instructions::{Instruction, OpCode},
    value::Value,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Absolute Stack Index
struct StkIdx(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Instruction Pointer
struct InstPtr(usize);

#[derive(Debug, Clone, Default)]
enum Target {
    #[default]
    Temp,
    Lit,
    Local(String),
}

#[derive(Default)]
pub struct Compiler {
    literals: Vec<Value>,
    instructions: Vec<Instruction>,
    target_stack: Vec<Target>,
    funcs: HashMap<String, FnByteCode>,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_bytecode(self) -> ByteCode {
        let mut funcs: HashMap<_, _> = global_functions()
            .into_iter()
            .filter_map(|(name, f)| {
                if let FnDecl::Native(f) = f {
                    Some((name, FnDef::Native(f)))
                } else {
                    None
                }
            })
            .collect();
        for (key, value) in self.funcs {
            funcs.insert(key, FnDef::User(Rc::new(value)));
        }

        ByteCode { funcs }
    }

    fn stack_top(&self) -> StkIdx {
        let Some(top) = self.target_stack.len().checked_sub(1) else {
            eprintln!("Target stack underflow during compilation! Compiled bytecode so far:");
            disasm_common(&self.literals, &self.instructions, &mut std::io::stderr()).unwrap();
            panic!();
        };
        StkIdx(top)
    }

    fn add_literal(&mut self, value: Value) -> u8 {
        let existing = self
            .literals
            .iter()
            .enumerate()
            .find(|(_, val)| **val == value);
        if let Some((i, _)) = existing {
            i as u8
        } else {
            let ret = self.literals.len();
            self.literals.push(value);
            ret as u8
        }
    }

    /// Returns absolute position of inserted value
    fn add_inst(&mut self, op: OpCode, arg0: u8) -> InstPtr {
        let inst = self.instructions.len();
        self.instructions.push(Instruction { op, arg0 });
        InstPtr(inst)
    }

    fn add_copy_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
        let inst = self.add_inst(
            OpCode::Copy,
            // convert bottom-to-top index into top-to-bottom one
            (self.target_stack.len() - stack_idx.0 - 1) as u8,
        );
        self.target_stack.push(Target::Temp);
        inst
    }

    fn add_load_literal_inst(&mut self, lit: u8) -> InstPtr {
        let inst = self.add_inst(OpCode::LoadLit, lit);
        self.target_stack.push(Target::Lit);
        inst
    }

    fn add_store_inst(&mut self, stack_idx: StkIdx) -> InstPtr {
        if self.target_stack.len() < stack_idx.0 + 1 {
            eprintln!("Target stack underflow during compilation! Compiled bytecode so far:");
            disasm_common(&self.literals, &self.instructions, &mut std::io::stderr()).unwrap();
            panic!();
        }
        let inst = self.add_inst(
            OpCode::Store,
            (self.target_stack.len() - stack_idx.0 - 1) as u8,
        );
        self.target_stack.pop();
        inst
    }

    fn add_jf_inst(&mut self) -> InstPtr {
        // Push with jump address 0, because it will be set later
        let inst = self.add_inst(OpCode::Jf, 0);
        self.target_stack.pop();
        inst
    }

    fn fixup_jmp(&mut self, ip: InstPtr) {
        self.instructions[ip.0].arg0 = self.instructions.len() as u8;
    }

    /// Pop until given stack index
    fn add_pop_until_inst(&mut self, stack_idx: StkIdx) -> Option<InstPtr> {
        if self.target_stack.len() <= stack_idx.0 {
            return None;
        }
        let inst = self.add_inst(
            OpCode::Pop,
            (self.target_stack.len() - stack_idx.0 - 1) as u8,
        );
        self.target_stack.resize(stack_idx.0 + 1, Target::Temp);
        Some(inst)
    }

    fn add_fn(&mut self, name: String, args: &[(Span, TypeDecl)], cofn: bool) {
        self.funcs.insert(
            name,
            FnByteCode::new(
                args.iter().map(|(arg, _)| arg.to_string()).collect(),
                std::mem::take(&mut self.literals),
                std::mem::take(&mut self.instructions),
                cofn,
            ),
        );
    }

    fn compile_expr(&mut self, ex: &Expression) -> Result<StkIdx, Box<dyn Error>> {
        Ok(match &ex.expr {
            ExprEnum::UndefinedLiteral => {
                let id = self.add_literal(Value::Undefined);
                self.add_load_literal_inst(id);
                self.stack_top()
            }
            ExprEnum::NullLiteral => {
                let id = self.add_literal(Value::Null);
                self.add_load_literal_inst(id);
                self.stack_top()
            }
            ExprEnum::BoolLiteral(b) => {
                let id = self.add_literal(Value::Bool(*b));
                self.add_load_literal_inst(id);
                self.stack_top()
            }
            ExprEnum::NumLiteral(num) => {
                let id = self.add_literal(Value::Num(*num));
                self.add_load_literal_inst(id);
                self.stack_top()
            }
            ExprEnum::BigIntLiteral(num) => {
                let id = self.add_literal(Value::Int(*num));
                self.add_load_literal_inst(id);
                self.stack_top()
            }
            ExprEnum::StrLiteral(str) => {
                let id = self.add_literal(Value::Str(str.clone()));
                self.add_load_literal_inst(id);
                self.stack_top()
            }
            ExprEnum::Ident(ident) => {
                let var = self.target_stack.iter().enumerate().find(|(_i, tgt)| {
                    if let Target::Local(id) = tgt {
                        id == ident.fragment()
                    } else {
                        false
                    }
                });
                if let Some(var) = var {
                    return Ok(StkIdx(var.0));
                } else {
                    return Err(format!("Variable not found: {ident:?}").into());
                }
            }
            ExprEnum::Not(ex) => {
                let ex = self.compile_expr(ex)?;
                self.add_copy_inst(ex);
                self.add_inst(OpCode::Not, 0);
                self.stack_top()
            }
            ExprEnum::BwNot(ex) => {
                let ex = self.compile_expr(ex)?;
                self.add_copy_inst(ex);
                self.add_inst(OpCode::BwNot, 0);
                self.stack_top()
            }
            ExprEnum::Minus(ex) => {
                let ex = self.compile_expr(ex)?;
                self.add_copy_inst(ex);
                self.add_inst(OpCode::Neg, 0);
                self.stack_top()
            }
            ExprEnum::Plus(ex) => self.compile_expr(ex)?,
            ExprEnum::Add(lhs, rhs) => self.bin_op(OpCode::Add, lhs, rhs)?,
            ExprEnum::Sub(lhs, rhs) => self.bin_op(OpCode::Sub, lhs, rhs)?,
            ExprEnum::Mul(lhs, rhs) => self.bin_op(OpCode::Mul, lhs, rhs)?,
            ExprEnum::Div(lhs, rhs) => self.bin_op(OpCode::Div, lhs, rhs)?,
            ExprEnum::Mod(lhs, rhs) => self.bin_op(OpCode::Mod, lhs, rhs)?,
            ExprEnum::Pow(lhs, rhs) => self.bin_op(OpCode::Pow, lhs, rhs)?,
            ExprEnum::BwOr(lhs, rhs) => self.bin_op(OpCode::BwOr, lhs, rhs)?,
            ExprEnum::BwAnd(lhs, rhs) => self.bin_op(OpCode::BwAnd, lhs, rhs)?,
            ExprEnum::BwXor(lhs, rhs) => self.bin_op(OpCode::BwXor, lhs, rhs)?,
            ExprEnum::BwLShift(lhs, rhs) => self.bin_op(OpCode::BwLShift, lhs, rhs)?,
            ExprEnum::BwRShift(lhs, rhs) => self.bin_op(OpCode::BwRShift, lhs, rhs)?,
            ExprEnum::BwRShiftU(lhs, rhs) => self.bin_op(OpCode::BwRShiftU, lhs, rhs)?,

            ExprEnum::Lt(lhs, rhs) => self.bin_op(OpCode::Lt, lhs, rhs)?,
            ExprEnum::Le(lhs, rhs) => self.bin_op(OpCode::Le, lhs, rhs)?,
            ExprEnum::Gt(lhs, rhs) => self.bin_op(OpCode::Gt, lhs, rhs)?,
            ExprEnum::Ge(lhs, rhs) => self.bin_op(OpCode::Ge, lhs, rhs)?,
            ExprEnum::Ee(lhs, rhs) => self.bin_op(OpCode::Ee, lhs, rhs)?,
            ExprEnum::Ne(lhs, rhs) => self.bin_op(OpCode::Ne, lhs, rhs)?,
            ExprEnum::Eee(lhs, rhs) => self.bin_op(OpCode::Eee, lhs, rhs)?,
            ExprEnum::Nee(lhs, rhs) => self.bin_op(OpCode::Nee, lhs, rhs)?,
            ExprEnum::FnInvoke(name, args) => {
                let stack_before_args = self.target_stack.len();
                let name_id = self.add_literal(Value::Str(name.to_string()));
                let args = args
                    .iter()
                    .map(|arg| self.compile_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let stack_before_call = self.target_stack.len();
                self.add_load_literal_inst(name_id);
                for arg in &args {
                    self.add_copy_inst(*arg);
                }

                self.add_inst(OpCode::Call, args.len() as u8);
                self.target_stack
                    .resize(stack_before_call + 1, Target::Temp);

                self.coerce_stack(StkIdx(stack_before_args));
                self.stack_top()
            }
            ExprEnum::Ternary {
                cond,
                true_branch,
                false_branch,
            } => {
                let cond = self.compile_expr(cond)?;
                self.add_copy_inst(cond);
                let jf_inst = self.add_jf_inst();
                let stack_size_before = self.target_stack.len();
                let _true_branch = self.compile_expr(true_branch)?;
                self.coerce_stack(StkIdx(stack_size_before + 1));
                let jmp_inst = self.add_inst(OpCode::Jmp, 0);
                self.fixup_jmp(jf_inst);
                self.target_stack.resize(stack_size_before, Target::Temp);
                let _false_branch = self.compile_expr(false_branch)?;
                self.coerce_stack(StkIdx(stack_size_before + 1));
                self.fixup_jmp(jmp_inst);
                self.stack_top()
            }
            ExprEnum::Satisfies(ex, _) => self.compile_expr(ex)?,
            ExprEnum::Await(ex) => {
                let res = self.compile_expr(ex)?;
                self.add_copy_inst(res);
                self.add_inst(OpCode::Await, 0);
                self.stack_top()
            }
            ExprEnum::Spread(_) => unreachable!("Spread operator should be handled in parser"),
            ExprEnum::Entry(_, _) => unreachable!("Entry should be handled in parser"),
        })
    }

    fn bin_op(
        &mut self,
        op: OpCode,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<StkIdx, Box<dyn Error>> {
        let lhs = self.compile_expr(lhs)?;
        let rhs = self.compile_expr(rhs)?;
        self.add_copy_inst(lhs);
        self.add_copy_inst(rhs);
        self.add_inst(op, 0);
        self.target_stack.pop();
        self.target_stack.pop();
        self.target_stack.push(Target::Temp);
        Ok(self.stack_top())
    }

    fn coerce_stack(&mut self, target: StkIdx) {
        match target.0.cmp(&(self.target_stack.len() - 1)) {
            std::cmp::Ordering::Less => {
                self.add_store_inst(target);
                self.add_pop_until_inst(target);
            }
            std::cmp::Ordering::Greater => {
                for _ in self.target_stack.len() - 1..target.0 {
                    self.add_copy_inst(self.stack_top());
                }
            }
            std::cmp::Ordering::Equal => {}
        }
    }

    fn compile_stmts(&mut self, stmts: &Statements) -> Result<Option<StkIdx>, Box<dyn Error>> {
        let mut last_result = None;
        for stmt in stmts {
            match stmt {
                Statement::Null => {}
                Statement::Expression(ex) => {
                    last_result = Some(self.compile_expr(ex)?);
                }
                Statement::Import { .. } => { /* TODO */ }
                Statement::ImportType { .. } => { /* TODO */ }
                Statement::VarDef { name, init, .. } => {
                    let undef_expr = Expression {
                        expr: ExprEnum::UndefinedLiteral,
                        span: Span::new(""),
                    };
                    let init = init.as_ref().unwrap_or(&undef_expr);
                    let mut init = self.compile_expr(init)?;
                    if !matches!(self.target_stack[init.0], Target::Temp) {
                        self.add_copy_inst(init);
                        init = self.stack_top();
                    }
                    self.target_stack[init.0] = Target::Local(name.to_string());
                    last_result = Some(init);
                }
                Statement::VarAssign { name, ex, .. } => {
                    let ex = self.compile_expr(ex)?;
                    let (stk_local, _) = self
                        .target_stack
                        .iter()
                        .enumerate()
                        .find(|(_, tgt)| {
                            if let Target::Local(tgt) = tgt {
                                tgt == name.fragment()
                            } else {
                                false
                            }
                        })
                        .ok_or_else(|| format!("Variable name not found: {name}"))?;
                    self.add_copy_inst(ex);
                    self.add_store_inst(StkIdx(stk_local));
                    last_result = Some(ex);
                }
                Statement::Block(stmts) => {
                    last_result = Some(self.compile_stmts_or_none(stmts)?);
                }
                Statement::If {
                    cond,
                    true_branch,
                    false_branch,
                } => {
                    let cond = self.compile_expr(cond)?;
                    self.add_copy_inst(cond);
                    let jf_inst = self.add_jf_inst();
                    let stack_size_before = self.target_stack.len();
                    let Statement::Block(true_branch) = &**true_branch else {
                        unreachable!()
                    };
                    self.compile_stmts_or_none(true_branch)?;
                    self.coerce_stack(StkIdx(stack_size_before + 1));
                    let jmp_inst = self.add_inst(OpCode::Jmp, 0);
                    self.fixup_jmp(jf_inst);
                    self.target_stack.resize(stack_size_before, Target::Temp);
                    if let Some(false_branch) = false_branch.as_ref() {
                        // false branch may be a If statement or a Block statement.
                        match &**false_branch {
                            Statement::Block(false_branch) => {
                                self.compile_stmts_or_none(false_branch)?;
                            }
                            Statement::If { .. } => {
                                self.compile_stmts_or_none(&vec![(**false_branch).clone()])?;
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.coerce_stack(StkIdx(stack_size_before + 1));
                    self.fixup_jmp(jmp_inst);
                }
                Statement::FnDef {
                    name,
                    args,
                    stmts,
                    is_cofn,
                    ..
                } => {
                    let literals = std::mem::take(&mut self.literals);
                    let instructions = std::mem::take(&mut self.instructions);
                    let target_stack = std::mem::take(&mut self.target_stack);
                    self.target_stack = args
                        .iter()
                        .map(|arg| Target::Local(arg.0.to_string()))
                        .collect();
                    self.compile_stmts(stmts)?;
                    self.add_fn(name.to_string(), args, *is_cofn);
                    self.literals = literals;
                    self.instructions = instructions;
                    self.target_stack = target_stack;
                }
                Statement::Return(ex) => {
                    let res = self.compile_expr(ex)?;
                    self.add_inst(OpCode::Ret, (self.target_stack.len() - res.0 - 1) as u8);
                }
                Statement::Yield(ex) => {
                    let res = self.compile_expr(ex)?;
                    self.add_inst(OpCode::Yield, (self.target_stack.len() - res.0 - 1) as u8);
                    self.target_stack.pop();
                }
                Statement::ExportDefault(ex) => {
                    // `export default expr` is a syntactic sugar for `export const default = expr`.
                    let res = self.compile_expr(ex)?;
                    self.add_copy_inst(res);

                    let name_id = self.add_literal(Value::Str("default".to_string()));
                    self.add_load_literal_inst(name_id);
                    self.add_inst(OpCode::Export, 0);
                }
                Statement::Export(stmts) => {
                    assert!(stmts.len() == 1);

                    self.compile_stmts(stmts)?;
                    match stmts[0] {
                        Statement::VarDef { name, .. } => {
                            let name_id = self.add_literal(Value::Str(name.to_string()));
                            self.add_inst(OpCode::LoadLit, name_id);
                        }
                        Statement::FnDef { name, .. } => {
                            return Err(format!("Function export not supported: '{name}'").into())
                        }
                        _ => {
                            unreachable!();
                        }
                    }
                    self.add_inst(OpCode::Export, 0);
                }
                Statement::Type { .. } => {
                    /* TODO: this interpreter may check types in runtime */
                }
            }
        }
        Ok(last_result)
    }

    fn compile_stmts_or_none(&mut self, stmts: &Statements) -> Result<StkIdx, Box<dyn Error>> {
        Ok(self.compile_stmts(stmts)?.unwrap_or_else(|| {
            let id = self.add_literal(Default::default());
            self.add_load_literal_inst(id);
            self.stack_top()
        }))
    }

    pub fn compile(&mut self, stmts: &Statements) -> Result<(), Box<dyn std::error::Error>> {
        let name = "main";
        self.compile_stmts_or_none(stmts)?;
        self.add_fn(name.to_string(), &[], false);
        Ok(())
    }

    pub fn disasm(&self, writer: &mut impl Write) -> std::io::Result<()> {
        for (name, fn_def) in &self.funcs {
            if fn_def.cofn {
                writeln!(writer, "Coroutine {name:?}:")?;
            } else {
                writeln!(writer, "Function {name:?}:")?;
            }
            fn_def.disasm(writer)?;
        }
        Ok(())
    }
}

fn disasm_common(
    literals: &[Value],
    instructions: &[Instruction],
    writer: &mut impl Write,
) -> std::io::Result<()> {
    use OpCode::*;
    writeln!(writer, "  Literals [{}]", literals.len())?;
    for (i, con) in literals.iter().enumerate() {
        writeln!(writer, "    [{i}] {}", *con)?;
    }

    writeln!(writer, "  Instructions [{}]", instructions.len())?;
    for (i, inst) in instructions.iter().enumerate() {
        match inst.op {
            LoadLit => writeln!(
                writer,
                "    [{i}] {:?} {} ({:?})",
                inst.op, inst.arg0, literals[inst.arg0 as usize]
            )?,
            Copy | Call | Jmp | Jf | Pop | Store | Ret => {
                writeln!(writer, "    [{i}] {:?} {}", inst.op, inst.arg0)?
            }
            _ => writeln!(writer, "    [{i}] {:?}", inst.op)?,
        }
    }
    Ok(())
}
