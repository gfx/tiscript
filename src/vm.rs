use std::{cell::RefCell, error::Error, rc::Rc};

use crate::{
    bytecode::{ByteCode, FnByteCode, FnDef},
    // dprintln,
    instructions::{Instruction, OpCode},
    value::Value,
};

pub enum YieldResult {
    Finished(Value),
    Suspend(Value),
}

pub struct StackFrame {
    fn_def: Rc<FnByteCode>,
    args: usize,
    stack: Vec<Value>,
    ip: usize,
}

impl StackFrame {
    fn new(fn_def: Rc<FnByteCode>, args: Vec<Value>) -> Self {
        Self {
            fn_def,
            args: args.len(),
            stack: args,
            ip: 0,
        }
    }

    fn inst(&self) -> Option<Instruction> {
        let ret = self.fn_def.instructions.get(self.ip)?;
        // dprintln!(
        //   "interpret[{}]: {:?} stack: {:?}",
        //   self.ip,
        //   ret,
        //   self.stack
        // );
        Some(*ret)
    }
}

pub struct Vm {
    bytecode: Rc<ByteCode>,
    stack_frames: Vec<StackFrame>,
    user_data: Box<dyn std::any::Any>,
    pub exports: Value, // must be an object
    debug_output: bool,
}

impl std::fmt::Debug for Vm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Vm>")
    }
}

fn err_bin_op(op: &str, lhs: &Value, rhs: &Value) -> Box<dyn Error> {
    format!(
        "Operator {} cannot be applied to types '{}' and '{}'",
        op,
        lhs.kind(),
        rhs.kind()
    )
    .into()
}

fn bin_op_add(lhs: &Value, rhs: &Value) -> Result<Value, Box<dyn Error>> {
    match (lhs, rhs) {
        (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs + rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs + rhs)),
        (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Str(lhs.clone() + rhs)),
        (Value::Str(lhs), rhs) => Ok(Value::Str(lhs.clone() + &rhs.to_string())),
        (lhs, Value::Str(rhs)) => Ok(Value::Str(lhs.to_string() + rhs)),
        _ => Err(err_bin_op("+", lhs, rhs)),
    }
}

fn bin_op_sub(lhs: &Value, rhs: &Value) -> Result<Value, Box<dyn Error>> {
    match (lhs, rhs) {
        (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs - rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
        _ => Err(err_bin_op("-", lhs, rhs)),
    }
}

fn bin_op_mul(lhs: &Value, rhs: &Value) -> Result<Value, Box<dyn Error>> {
    match (lhs, rhs) {
        (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs * rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
        _ => Err(err_bin_op("*", lhs, rhs)),
    }
}

fn bin_op_div(lhs: &Value, rhs: &Value) -> Result<Value, Box<dyn Error>> {
    match (lhs, rhs) {
        (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs / rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
        _ => Err(err_bin_op("/", lhs, rhs)),
    }
}

fn bin_op_mod(lhs: &Value, rhs: &Value) -> Result<Value, Box<dyn Error>> {
    match (lhs, rhs) {
        (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs % rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
        _ => Err(err_bin_op("%", lhs, rhs)),
    }
}

fn bin_op_lt(lhs: &Value, rhs: &Value) -> Result<Value, Box<dyn Error>> {
    match (lhs, rhs) {
        (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Bool(*lhs < *rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs < *rhs)),
        (Value::Num(lhs), Value::Int(rhs)) => Ok(Value::Bool(*lhs < *rhs as f64)),
        (Value::Int(lhs), Value::Num(rhs)) => Ok(Value::Bool((*lhs as f64) < *rhs)),
        (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(*lhs < *rhs)),
        _ => Err(err_bin_op("<", lhs, rhs)),
    }
}

fn stack_overflow(ip: usize, instruction: Instruction, stack: &Vec<Value>) -> ! {
    panic!(
        "[BUG] Stack overflow in `[{}] {:?}` where stack.len() is {} (see --disasm to to look into the instruction)",
        ip, instruction, stack.len()
    );
}

impl Vm {
    pub fn new(
        bytecode: Rc<ByteCode>,
        user_data: Box<dyn std::any::Any>,
        debug_output: bool,
    ) -> Self {
        Self {
            bytecode,
            stack_frames: Default::default(),
            user_data,
            exports: Value::Object(Default::default()),
            debug_output,
        }
    }

    pub fn top(&self) -> Result<&StackFrame, String> {
        self.stack_frames
            .last()
            .ok_or_else(|| "Stack frame underflow".to_string())
    }

    fn top_mut(&mut self) -> Result<&mut StackFrame, String> {
        self.stack_frames
            .last_mut()
            .ok_or_else(|| "Stack frame underflow".to_string())
    }

    /// A convenience function to run a function without the
    /// ability to suspend execution.
    /// An yield instruction would be an error.
    #[allow(dead_code)]
    fn run_fn(&mut self, fn_name: &str, args: &[Value]) -> Result<Value, Box<dyn Error>> {
        let fn_def = self
            .bytecode
            .funcs
            .get(fn_name)
            .ok_or_else(|| format!("Function {fn_name:?} was not found"))?;
        let fn_def = match fn_def {
            FnDef::User(user) => user.clone(),
            FnDef::Native(n) => return (*n.code)(self.user_data.as_ref(), args),
        };

        self.stack_frames
            .push(StackFrame::new(fn_def, args.to_vec()));

        match self.interpret()? {
            YieldResult::Finished(val) => Ok(val),
            YieldResult::Suspend(_) => Err("Yielded at toplevel".into()),
        }
    }

    pub fn init_fn(&mut self, fn_name: &str, args: &[Value]) -> Result<(), Box<dyn Error>> {
        let fn_def = self
            .bytecode
            .funcs
            .get(fn_name)
            .ok_or_else(|| format!("Function {fn_name:?} was not found"))?;
        let fn_def = match fn_def {
            FnDef::User(user) => user.clone(),
            FnDef::Native(_) => {
                return Err(
                    "Native function cannot be called as a coroutine. Use `run_fn` instead.".into(),
                )
            }
        };

        self.stack_frames
            .push(StackFrame::new(fn_def, args.to_vec()));

        Ok(())
    }

    fn return_fn(&mut self, stack_pos: u8) -> Result<Option<YieldResult>, Box<dyn Error>> {
        let top_frame = self
            .stack_frames
            .pop()
            .ok_or("Stack frame underflow at Ret")?;
        let res = top_frame
            .stack
            .get(top_frame.stack.len() - stack_pos as usize - 1)
            .ok_or("Stack underflow at Ret")?
            .clone();
        let args = top_frame.args;

        if self.stack_frames.is_empty() {
            return Ok(Some(YieldResult::Finished(res)));
        }

        if self.debug_output {
            eprintln!("Returning {}", res);
        }

        let stack = &mut self.top_mut()?.stack;
        stack.resize(stack.len() - args - 1, Value::Num(0.));
        stack.push(res);
        self.top_mut()?.ip += 1;
        Ok(None)
    }

    pub fn interpret(&mut self) -> Result<YieldResult, Box<dyn Error>> {
        loop {
            let (instruction, ip) = if let Some(instruction) = self.top()?.inst() {
                (instruction, self.top()?.ip)
            } else {
                if let Some(res) = self.return_fn(0)? {
                    return Ok(res);
                }
                continue;
            };

            if self.debug_output {
                eprintln!(
                    "interpret[{ip}]: {instruction:?} stack: {stack:?}",
                    stack = self.top()?.stack
                );
            }

            match instruction.op {
                OpCode::Nop => (),
                OpCode::LoadLit => {
                    let stack_frame = self.top_mut()?;
                    stack_frame
                        .stack
                        .push(stack_frame.fn_def.literals[instruction.arg0 as usize].clone());
                }
                OpCode::Store => {
                    let stack = &mut self.top_mut()?.stack;
                    let Some(d) = stack.len().checked_sub(instruction.arg0 as usize) else {
                        stack_overflow(ip, instruction, stack);
                    };
                    let Some(idx) = d.checked_sub(1) else {
                        stack_overflow(ip, instruction, stack);
                    };
                    let value = stack.pop().expect("Store needs an argument");
                    stack[idx] = value;
                }
                OpCode::Copy => {
                    let stack = &mut self.top_mut()?.stack;
                    let Some(d) = stack.len().checked_sub(instruction.arg0 as usize) else {
                        stack_overflow(ip, instruction, stack);
                    };
                    let Some(idx) = d.checked_sub(1) else {
                        stack_overflow(ip, instruction, stack);
                    };
                    stack.push(stack[idx].clone());
                }
                OpCode::Dup => {
                    let stack = &mut self.top_mut()?.stack;
                    let top = stack.last().unwrap().clone();
                    stack.extend((0..instruction.arg0).map(|_| top.clone()));
                }
                OpCode::Not => {
                    let stack = &mut self.top_mut()?.stack;
                    let top = stack.pop().expect("Not needs an argument");
                    stack.push(Value::Bool(!top.to_bool()));
                }
                OpCode::Neg => {
                    let stack = &mut self.top_mut()?.stack;
                    let top = stack.pop().expect("Neg needs an argument");
                    match top {
                        Value::Num(n) => stack.push(Value::Num(-n)),
                        Value::Int(n) => stack.push(Value::Int(-n)),
                        _ => panic!("Neg needs a number"),
                    }
                }
                OpCode::Add => Self::interpret_bin_op(&mut self.top_mut()?.stack, bin_op_add)?,
                OpCode::Sub => Self::interpret_bin_op(&mut self.top_mut()?.stack, bin_op_sub)?,
                OpCode::Mul => Self::interpret_bin_op(&mut self.top_mut()?.stack, bin_op_mul)?,
                OpCode::Div => Self::interpret_bin_op(&mut self.top_mut()?.stack, bin_op_div)?,
                OpCode::Mod => Self::interpret_bin_op(&mut self.top_mut()?.stack, bin_op_mod)?,
                OpCode::Call => {
                    let stack = &self.top()?.stack;
                    let args = &stack[stack.len() - instruction.arg0 as usize..];
                    let fname = &stack[stack.len() - instruction.arg0 as usize - 1];
                    let Value::Str(fname) = fname else {
                        panic!(
                            "Function name shall be a string: {fname:?} in fn {:?}",
                            self.top()?.stack
                        );
                    };
                    let fn_def = self
                        .bytecode
                        .funcs
                        .get(fname)
                        .ok_or_else(|| format!("Function not found: {fname:?}"))?;
                    match fn_def {
                        FnDef::User(user_fn) => {
                            if user_fn.cofn {
                                let mut vm =
                                    Vm::new(self.bytecode.clone(), Box::new(()), self.debug_output);
                                vm.stack_frames
                                    .push(StackFrame::new(user_fn.clone(), args.to_vec()));
                                let stack = &mut self.top_mut()?.stack;
                                stack.resize(
                                    stack.len() - instruction.arg0 as usize - 1,
                                    Value::Num(0.),
                                );
                                stack.push(Value::Coro(Rc::new(RefCell::new(vm))));
                            } else {
                                self.stack_frames
                                    .push(StackFrame::new(user_fn.clone(), args.to_vec()));
                                continue;
                            }
                        }
                        FnDef::Native(native) => {
                            let res = (native.code)(self.user_data.as_ref(), args)?;
                            let stack = &mut self.top_mut()?.stack;
                            stack.resize(
                                stack.len() - instruction.arg0 as usize - 1,
                                Value::Num(0.),
                            );
                            stack.push(res);
                        }
                    }
                }
                OpCode::Jmp => {
                    self.top_mut()?.ip = instruction.arg0 as usize;
                    continue;
                }
                OpCode::Jf => {
                    let stack = &mut self.top_mut()?.stack;
                    let cond = stack.pop().expect("Jf needs an argument");
                    if cond.to_bool() {
                        self.top_mut()?.ip = instruction.arg0 as usize;
                        continue;
                    }
                }
                OpCode::Lt => Self::interpret_bin_op(&mut self.top_mut()?.stack, bin_op_lt)?,
                OpCode::Pop => {
                    let stack = &mut self.top_mut()?.stack;
                    stack.resize(stack.len() - instruction.arg0 as usize, Value::default());
                }
                OpCode::Ret => {
                    if let Some(res) = self.return_fn(instruction.arg0)? {
                        return Ok(res);
                    }
                    continue;
                }
                OpCode::Yield => {
                    let top_frame = self.top_mut()?;
                    let res = top_frame
                        .stack
                        .pop()
                        .ok_or_else(|| "Stack underflow".to_string())?;
                    // Increment the ip for the next call
                    top_frame.ip += 1;
                    return Ok(YieldResult::Suspend(res));
                }
                OpCode::Await => {
                    let vms = self
                        .top_mut()?
                        .stack
                        .pop()
                        .ok_or_else(|| "Stack underflow".to_string())?;
                    let Value::Coro(vm) = vms else {
                        return Err("Await keyword applied to a non-coroutine".into());
                    };
                    match vm.borrow_mut().interpret() {
                        Ok(YieldResult::Finished(_)) => (),
                        Ok(YieldResult::Suspend(value)) => {
                            self.top_mut()?.stack.push(value);
                        }
                        Err(e) => {
                            eprintln!("Runtime error: {e:?}");
                        }
                    };
                }
                OpCode::Export => {
                    let stack = &mut self.top_mut()?.stack;

                    let name = stack
                        .pop()
                        .expect("Export needs a name")
                        .must_be_str()
                        .expect("Export name must be a string")
                        .to_string();
                    let value = stack.pop().expect("Export needs a value");

                    match self.exports {
                        Value::Object(ref mut map) => {
                            map.insert(name, value);
                        }
                        _ => {
                            panic!("vm.exports must be an object");
                        }
                    }
                }
            }
            self.top_mut()?.ip += 1;
        }
    }

    fn interpret_bin_op(
        stack: &mut Vec<Value>,
        op: impl FnOnce(&Value, &Value) -> Result<Value, Box<dyn Error>>,
    ) -> Result<(), Box<dyn Error>> {
        let rhs = stack.pop().expect("Stack underflow");
        let lhs = stack.pop().expect("Stack underflow");
        let res = op(&lhs, &rhs)?;
        stack.push(res);
        Ok(())
    }

    fn back_trace(&self) {
        for (i, frame) in self.stack_frames.iter().rev().enumerate() {
            eprintln!("[{}]: {:?}", i, frame.stack);
        }
    }
}
