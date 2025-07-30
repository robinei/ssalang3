use ir::Instr;

mod compile;

pub use compile::*;

#[derive(Copy, Clone)]
struct BinopArgs {
    lhs: u32,
    rhs: u32,
}

#[derive(Copy, Clone)]
struct BranchArgs {
    cond: u32,
    target: u16,
    else_target: u16,
}

#[derive(Copy, Clone)]
union Value {
    bool_value: bool,
    i32_value: i32,
    i64_value: i64,
    u32_value: u32,
    u64_value: u64,
    f32_value: f32,
    f64_value: f64,
    binop: BinopArgs,
    branch: BranchArgs,
}

type InstrFunc = fn(vm: &mut VMState, data: Value) -> ();

#[derive(Copy, Clone)]
struct VMInstr {
    func: InstrFunc,
    data: Value,
}

pub struct VMState {
    ip: usize,
    result: Option<Instr>,

    // these two are the same size. values will contain the last result of the corresponding instruction in code
    code: Vec<VMInstr>,
    values: Vec<Value>,

    // mapping from BlockRef to instruction position
    block_offsets: Vec<u32>,
}

impl VMState {
    pub fn execute(&mut self) -> Option<Instr> {
        self.ip = 0;
        self.result = None;
        while self.ip < self.code.len() {
            let instr = self.code[self.ip];
            (instr.func)(self, instr.data)
        }
        self.result
    }
}
