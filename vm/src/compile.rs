use common::{Type, TypeId};
use ir::Instr;

use crate::{InstrFunc, VMInstr, Value};

pub fn compile_ir(ir_code: Vec<Instr>) {
    let mut vm_code = Vec::new();
    let mut block_offsets = Vec::new();
    let mut values = Vec::<Value>::new();

    values.resize(ir_code.len(), Value { u64_value: 0 });

    for (ip, instr) in ir_code.iter().enumerate() {
        let mut data = Value { u64_value: 0 };

        let func: InstrFunc = match instr {
            Instr::ConstBool(_, _value) => |_vm, _data| {},
            Instr::Add(meta, lhs, rhs) => {
                data.binop.lhs = lhs.get() as u32;
                data.binop.rhs = rhs.get() as u32;
                match meta.get_type_id() {
                    TypeId::I32 => |vm, data| {
                        vm.values[vm.ip].i32_value =
                            unsafe { vm.values[data.binop.lhs as usize].i32_value + vm.values[data.binop.rhs as usize].i32_value };
                        vm.ip += 1;
                    },
                    _ => unreachable!(),
                }
            }
            Instr::Label(_, block) => {
                let block = block.get() as usize;
                if block >= block_offsets.len() {
                    block_offsets.resize(block, 0);
                }
                block_offsets[block] = ip as u32 + 1;
                |_, _| {}
            }
            Instr::Jump(_, target) => {
                data.branch.target = target.get() as u16;
                |vm, data| {
                    vm.ip = vm.block_offsets[unsafe { data.branch.target } as usize] as usize;
                }
            }
            Instr::Branch(_, cond, target, else_target) => {
                data.branch.cond = cond.get() as u32;
                data.branch.target = target.get() as u16;
                data.branch.else_target = else_target.get() as u16;
                |vm, data| {
                    vm.ip = vm.block_offsets[unsafe {
                        if vm.values[data.branch.cond as usize].bool_value {
                            data.branch.target
                        } else {
                            data.branch.else_target
                        }
                    } as usize] as usize;
                }
            }
            Instr::Ret(meta, value) => {
                match *meta.get_type() {
                    Type::Int(_, _) => {
                        data.i64_value = value.get() as i64;
                        |vm, data| {
                            vm.result = Some(Instr::const_int(unsafe { vm.values[data.i64_value as usize].i64_value }));
                            vm.ip = vm.code.len(); // out of bounds ends dispatch loop
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };

        vm_code.push(VMInstr { func, data });
    }
}
