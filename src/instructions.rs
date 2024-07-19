#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Nop,
    // Load a literal from the literal table.
    LoadLit,
    Store,
    // Push the value of the index arg0 in the stack to the top of the stack.
    Copy,
    /// Duplicate the value on the top of the stack arg0 times
    Dup,
    Not,
    BwNot,
    Neg,
    Add,
    Sub,
    Mul,
    Mod,
    Div,
    BwOr,
    BwAnd,
    BwXor,
    BwLShift,
    BwRShift,
    BwRShiftU,
    Call,
    Jmp,
    /// Jump if false
    Jf,
    /// Pop a value from the stack, compare it with a value at arg0, push true if it's less
    Lt, // <
    Le,  // <=
    Gt,  // >
    Ge,  // >=
    Ee,  // ==
    Ne,  // !=
    Eee, // ===
    Nee, // !==
    /// Pop n values from the stack where n is given by arg0
    Pop,
    /// Return current function
    Ret,
    /// Suspend current function execution where it can resume later.
    Yield,
    /// Await a coroutine in progress until the next yield
    Await,
    /// Export a symbol in a statement
    Export,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Instruction {
    pub(crate) op: OpCode,
    pub(crate) arg0: u8,
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {})", self.op, self.arg0)
    }
}
