use crate::Code;
use crate::refmap::RefMap;
use crate::{BlockRef, Instr, InstrRef, Meta, PhiRef, VarRef};
use common::TypeId;
use smallvec::SmallVec;
use std::collections::HashMap;

#[derive(Debug)]
struct Variable {
    ty: TypeId,
}

impl Default for Variable {
    fn default() -> Self {
        Self {
            ty: TypeId::unit_id(),
        }
    }
}

#[derive(Debug)]
struct BasicBlock {
    sealed: bool,
    //visited: bool,
    first: Option<InstrRef>,
    last: Option<InstrRef>,
    //idom: Option<BlockRef>,
    //dom_depth: u16,
    //postorder: u16,
    //loop_depth: u16,
    succs: [Option<BlockRef>; 2],
    preds: SmallVec<[BlockRef; 8]>,
    incomplete_phis: SmallVec<[PhiRef; 8]>,
    suffix: SmallVec<[InstrRef; 8]>,
}

impl BasicBlock {
    fn new() -> Self {
        Self {
            sealed: false,
            //visited: false,
            first: None,
            last: None,
            //idom: None,
            //dom_depth: 0,
            //postorder: 0,
            //loop_depth: 0,
            succs: [None; 2],
            preds: SmallVec::new(),
            incomplete_phis: SmallVec::new(),
            suffix: SmallVec::new(),
        }
    }
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct Phi {
    var: Option<VarRef>,
    instr: Option<InstrRef>,
    upsilons: SmallVec<[InstrRef; 8]>,
}

impl Phi {
    fn new() -> Self {
        Self {
            var: None,
            instr: None,
            upsilons: SmallVec::new(),
        }
    }
}

impl Default for Phi {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct IrBuilder {
    code: Code,
    curr_block: Option<BlockRef>,
    blocks: RefMap<BlockRef, BasicBlock>,
    vars: RefMap<VarRef, Variable>,
    phis: RefMap<PhiRef, Phi>,
    bindings: HashMap<(BlockRef, VarRef), Instr>,
    ircache: HashMap<Instr, InstrRef>,
}

impl IrBuilder {
    pub fn new() -> Self {
        Self {
            code: Code::with_capacity(16, 16),
            curr_block: None,
            blocks: RefMap::new(), // RefMap automatically reserves index 0
            vars: RefMap::new(),   // RefMap automatically reserves index 0
            phis: RefMap::new(),   // RefMap automatically reserves index 0
            bindings: HashMap::new(),
            ircache: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.code = Code::with_capacity(16, 16);
        self.curr_block = None;
        self.blocks.clear(); // RefMap::clear automatically preserves index 0
        self.vars.clear(); // RefMap::clear automatically preserves index 0
        self.phis.clear(); // RefMap::clear automatically preserves index 0
        self.bindings.clear();
        self.ircache.clear();
    }

    pub fn create_block(&mut self) -> BlockRef {
        self.blocks.push(BasicBlock::new())
    }

    pub fn create_variable(&mut self, ty: TypeId) -> VarRef {
        assert_ne!(ty, TypeId::unit_id());
        self.vars.push(Variable { ty })
    }

    pub fn create_phi(&mut self) -> PhiRef {
        self.phis.push(Phi::new())
    }

    pub fn get_current_block(&self) -> Option<BlockRef> {
        self.curr_block
    }

    pub fn is_block_terminated(&self, block: BlockRef) -> bool {
        self.blocks.get(block).last.is_some()
    }

    fn create_phi_var(&mut self, var: VarRef) -> PhiRef {
        let mut phi = Phi::new();
        phi.var = Some(var);
        self.phis.push(phi)
    }

    fn emit_instr_pinned(&mut self, instr: Instr) -> InstrRef {
        self.code.push_pinned(instr)
    }

    fn try_lookup_instr(&self, instr: &Instr) -> Option<InstrRef> {
        self.ircache.get(instr).copied()
    }

    fn emit_instr_unpinned(&mut self, instr: Instr) -> InstrRef {
        if instr.is_pure() {
            if let Some(cached) = self.try_lookup_instr(&instr) {
                return cached;
            }
        }

        let instr_ref = self.code.push_unpinned(instr);

        if instr.is_pure() {
            self.ircache.insert(instr, instr_ref);
        }

        instr_ref
    }

    fn append_block_instr(&mut self, block: BlockRef, instr: Instr) -> InstrRef {
        let is_current_block = self.curr_block == Some(block);

        // Check if we can emit directly as pinned
        if is_current_block && self.blocks.get(block).last.is_none() {
            assert!(!instr.is_pure());
            return self.emit_instr_pinned(instr);
        }

        let instr_ref = self.emit_instr_unpinned(instr);

        // Now update the suffix list
        self.blocks.get_mut(block).suffix.push(instr_ref);

        instr_ref
    }

    pub fn to_instr(&self, instr_ref: InstrRef) -> Instr {
        let instr = self.code[instr_ref];
        match instr {
            Instr::ConstBool(..) | Instr::ConstI32(..) | Instr::Identity(..) => instr,
            _ => Instr::Identity(Meta::new(instr.get_type_id()), instr_ref),
        }
    }

    pub fn intern_instr(&mut self, instr: Instr) -> InstrRef {
        match instr {
            Instr::Identity(_, instr_ref) => instr_ref,
            _ => {
                assert!(instr.is_pure());
                self.emit_instr_unpinned(instr)
            }
        }
    }

    pub fn print(&mut self, val: Instr) {
        let instr_ref = self.intern_instr(val);
        self.emit_instr_pinned(Instr::Print(Meta::new(TypeId::unit_id()), instr_ref));
    }

    pub fn label(&mut self, block: BlockRef) {
        assert!(
            self.curr_block.is_none() || self.blocks.get(self.curr_block.unwrap()).last.is_some()
        );
        {
            let b = self.blocks.get(block);
            assert!(b.first.is_none());
            assert!(b.last.is_none());
            assert!(b.succs[0].is_none());
            assert!(b.succs[1].is_none());
        }

        let instr_ref = self.emit_instr_pinned(Instr::Label(Meta::new(TypeId::unit_id()), block));
        self.blocks.get_mut(block).first = Some(instr_ref);
        self.curr_block = Some(block);
    }

    pub fn jump(&mut self, target: BlockRef) {
        self.blocks.get_mut(target);

        let curr_block = self.curr_block.expect("No current block for jump");

        {
            let b = self.blocks.get(curr_block);
            let tb = self.blocks.get(target);
            assert!(b.last.is_none());
            assert!(b.succs[0].is_none());
            assert!(!tb.sealed);
        }

        let jump_instr = self.emit_instr_pinned(Instr::Jump(Meta::new(TypeId::unit_id()), target));

        self.blocks.get_mut(target).preds.push(curr_block);

        let b = self.blocks.get_mut(curr_block);
        b.succs[0] = Some(target);
        b.last = Some(jump_instr);
    }

    pub fn branch(&mut self, cond: Instr, true_target: BlockRef, false_target: BlockRef) {
        let curr_block = self.curr_block.expect("No current block for branch");

        // Validate state
        {
            let b = self.blocks.get(curr_block);
            assert!(b.last.is_none());
            assert!(b.succs[0].is_none());
            assert!(b.succs[1].is_none());
        }
        assert!(!self.blocks.get_mut(true_target).sealed);
        assert!(!self.blocks.get_mut(false_target).sealed);
        assert_eq!(cond.get_type_id(), TypeId::bool_id());

        // Optimize constant branches
        if let Instr::ConstBool(_, val) = cond {
            self.jump(if val { true_target } else { false_target });
            return;
        }

        // Optimize same target branches
        if true_target == false_target {
            self.jump(true_target);
            return;
        }

        let cond_ref = self.intern_instr(cond);
        let branch_instr = self.emit_instr_pinned(Instr::Branch(
            Meta::new(TypeId::unit_id()),
            cond_ref,
            true_target,
            false_target,
        ));

        self.blocks.get_mut(true_target).preds.push(curr_block);
        self.blocks.get_mut(false_target).preds.push(curr_block);

        let b = self.blocks.get_mut(curr_block);
        b.succs[0] = Some(true_target);
        b.succs[1] = Some(false_target);
        b.last = Some(branch_instr);
    }

    pub fn ret(&mut self, retval: Instr) {
        let curr_block = self.curr_block.expect("No current block for ret");
        assert!(self.blocks.get(curr_block).last.is_none());

        let retval_ref = self.intern_instr(retval);
        let ret_instr =
            self.emit_instr_pinned(Instr::Ret(Meta::new(TypeId::unit_id()), retval_ref));
        self.blocks.get_mut(curr_block).last = Some(ret_instr);
    }

    pub fn upsilon(&mut self, block: BlockRef, phi: PhiRef, val: Instr) {
        let val_ref = self.intern_instr(val);
        let instr_ref = self.append_block_instr(
            block,
            Instr::Upsilon(Meta::new(TypeId::unit_id()), phi, val_ref),
        );

        self.phis.get_mut(phi).upsilons.push(instr_ref);
    }

    pub fn phi(&mut self, phi: PhiRef, ty: TypeId) -> Instr {
        assert_ne!(ty, TypeId::unit_id());
        assert!(self.phis.get_mut(phi).instr.is_none());

        let instr_ref = self.emit_instr_pinned(Instr::Phi(Meta::new(ty), phi));
        self.phis.get_mut(phi).instr = Some(instr_ref);

        Instr::Identity(Meta::new(ty), instr_ref)
    }

    pub fn arg(&self, arg: i32, ty: TypeId) -> Instr {
        assert!(arg >= 0);
        assert_ne!(ty, TypeId::unit_id());
        Instr::Arg(Meta::new(ty), arg)
    }

    pub fn add(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_i32(lval.wrapping_add(rval));
            }
            (Instr::ConstI32(_, 0), _) => {
                return rhs;
            }
            (_, Instr::ConstI32(_, 0)) => {
                return lhs;
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Add(Meta::new(lhs.get_type_id()), lhs_ref, rhs_ref)
    }

    pub fn eq(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstBool(_, lval), Instr::ConstBool(_, rval)) => {
                return Instr::const_bool(lval == rval);
            }
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_bool(lval == rval);
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Eq(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn neq(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstBool(_, lval), Instr::ConstBool(_, rval)) => {
                return Instr::const_bool(lval != rval);
            }
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_bool(lval != rval);
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Neq(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn sub(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_i32(lval.wrapping_sub(rval));
            }
            (_, Instr::ConstI32(_, 0)) => {
                return lhs; // x - 0 = x
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Sub(Meta::new(lhs.get_type_id()), lhs_ref, rhs_ref)
    }

    pub fn mul(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_i32(lval.wrapping_mul(rval));
            }
            (Instr::ConstI32(_, 0), _) | (_, Instr::ConstI32(_, 0)) => {
                return Instr::const_i32(0); // x * 0 = 0
            }
            (Instr::ConstI32(_, 1), _) => {
                return rhs; // 1 * x = x
            }
            (_, Instr::ConstI32(_, 1)) => {
                return lhs; // x * 1 = x
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Mul(Meta::new(lhs.get_type_id()), lhs_ref, rhs_ref)
    }

    pub fn div(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                if rval != 0 {
                    return Instr::const_i32(lval / rval);
                }
                // Division by zero - let runtime handle it
            }
            (_, Instr::ConstI32(_, 1)) => {
                return lhs; // x / 1 = x
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Div(Meta::new(lhs.get_type_id()), lhs_ref, rhs_ref)
    }

    pub fn lt(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_bool(lval < rval);
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Lt(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn gt(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_bool(lval > rval);
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Gt(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn lt_eq(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_bool(lval <= rval);
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::LtEq(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn gt_eq(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), rhs.get_type_id());

        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::const_bool(lval >= rval);
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::GtEq(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn and(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), TypeId::bool_id());
        assert_eq!(rhs.get_type_id(), TypeId::bool_id());

        // Constant folding and short-circuit optimizations
        match (lhs, rhs) {
            (Instr::ConstBool(_, lval), Instr::ConstBool(_, rval)) => {
                return Instr::const_bool(lval && rval);
            }
            (Instr::ConstBool(_, false), _) | (_, Instr::ConstBool(_, false)) => {
                return Instr::const_bool(false); // x && false = false
            }
            (Instr::ConstBool(_, true), _) => {
                return rhs; // true && x = x
            }
            (_, Instr::ConstBool(_, true)) => {
                return lhs; // x && true = x
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::And(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn or(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type_id(), TypeId::bool_id());
        assert_eq!(rhs.get_type_id(), TypeId::bool_id());

        // Constant folding and short-circuit optimizations
        match (lhs, rhs) {
            (Instr::ConstBool(_, lval), Instr::ConstBool(_, rval)) => {
                return Instr::const_bool(lval || rval);
            }
            (Instr::ConstBool(_, true), _) | (_, Instr::ConstBool(_, true)) => {
                return Instr::const_bool(true); // x || true = true
            }
            (Instr::ConstBool(_, false), _) => {
                return rhs; // false || x = x
            }
            (_, Instr::ConstBool(_, false)) => {
                return lhs; // x || false = x
            }
            _ => {}
        }

        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Or(Meta::new(TypeId::bool_id()), lhs_ref, rhs_ref)
    }

    pub fn neg(&mut self, operand: Instr) -> Instr {
        assert_eq!(operand.get_type_id(), TypeId::i32_id());

        // Constant folding
        match operand {
            Instr::ConstI32(_, val) => {
                return Instr::const_i32(val.wrapping_neg());
            }
            _ => {}
        }

        let operand_ref = self.intern_instr(operand);
        Instr::Neg(Meta::new(TypeId::i32_id()), operand_ref)
    }

    pub fn not(&mut self, operand: Instr) -> Instr {
        assert_eq!(operand.get_type_id(), TypeId::bool_id());

        // Constant folding
        match operand {
            Instr::ConstBool(_, val) => {
                return Instr::const_bool(!val);
            }
            _ => {}
        }

        let operand_ref = self.intern_instr(operand);
        Instr::Not(Meta::new(TypeId::bool_id()), operand_ref)
    }

    pub fn seal_block(&mut self, block: BlockRef) {
        let incomplete_phis: SmallVec<[PhiRef; 8]> = {
            let b = self.blocks.get_mut(block);
            assert!(!b.sealed);
            b.sealed = true;
            b.incomplete_phis.clone()
        };

        for phi_ref in incomplete_phis {
            self.create_pred_upsilons(block, phi_ref);
        }
    }

    fn create_pred_upsilons(&mut self, block: BlockRef, phi: PhiRef) -> Instr {
        let var = self
            .phis
            .get(phi)
            .var
            .expect("Phi should have associated variable");
        let phi_instr_ref = self
            .phis
            .get(phi)
            .instr
            .expect("Phi should have associated instruction");
        let phi_instr = self.to_instr(phi_instr_ref);

        // Collect values from all predecessors
        let preds = self.blocks.get(block).preds.clone();
        let mut values: SmallVec<[Instr; 8]> = SmallVec::new();
        let mut unique_non_phi_values: SmallVec<[Instr; 8]> = SmallVec::new();

        for pred in &preds {
            let val = self.read_variable(*pred, var);
            values.push(val);

            // Collect unique non-phi values (including resolved identities)
            let resolved_val = self.resolve_identity_chain(val);
            if resolved_val != phi_instr && !unique_non_phi_values.contains(&resolved_val) {
                unique_non_phi_values.push(resolved_val);
            }
        }

        assert!(
            !values.is_empty(),
            "Phi should have at least one incoming value"
        );

        // Phi elimination: if all non-phi values are the same, replace phi with that value
        if unique_non_phi_values.len() == 1 {
            let replacement = unique_non_phi_values[0];
            let candidate_ref = self.intern_instr(replacement);
            self.code.set(
                phi_instr_ref,
                Instr::Identity(Meta::new(replacement.get_type_id()), candidate_ref),
            );
            return replacement;
        }

        // Multiple different values or all values are phi itself - keep the phi
        // Create upsilons for all predecessors
        for (pred, val) in preds.into_iter().zip(values.into_iter()) {
            self.upsilon(pred, phi, val);
        }

        phi_instr
    }

    fn resolve_identity_chain(&self, mut instr: Instr) -> Instr {
        // Follow chains of Identity instructions to their final target
        // This helps with iterative phi elimination
        let mut visited = 0;
        const MAX_CHAIN_LENGTH: usize = 10; // Prevent infinite loops

        while let Instr::Identity(_, target_ref) = instr {
            if visited >= MAX_CHAIN_LENGTH {
                break; // Prevent infinite loops in malformed IR
            }
            instr = self.to_instr(target_ref);
            visited += 1;
        }

        instr
    }

    pub fn write_variable(&mut self, block: BlockRef, var: VarRef, val: Instr) {
        assert_ne!(val.get_type_id(), TypeId::unit_id());
        assert_eq!(val.get_type_id(), self.vars.get(var).ty);

        self.bindings.insert((block, var), val);
    }

    pub fn read_variable(&mut self, block: BlockRef, var: VarRef) -> Instr {
        if let Some(val) = self.bindings.get(&(block, var)) {
            return *val;
        }

        let var_type = self.vars.get(var).ty;
        let is_sealed = self.blocks.get(block).sealed;

        let result = if !is_sealed {
            // Create incomplete phi
            let phi = self.create_phi_var(var);
            self.blocks.get_mut(block).incomplete_phis.push(phi);
            self.phi(phi, var_type)
        } else {
            let pred_count = self.blocks.get(block).preds.len();
            if pred_count == 0 {
                // Entry block with no predecessors - variable is undefined
                // This should only happen for function arguments or uninitialized variables
                panic!(
                    "Reading undefined variable {} in entry block {}",
                    var.get(),
                    block.get()
                );
            } else if pred_count == 1 {
                // Exactly one predecessor
                self.read_variable(self.blocks.get(block).preds[0], var)
            } else {
                // Multiple predecessors - create phi
                let phi = self.create_phi_var(var);
                let result = self.phi(phi, var_type);
                self.write_variable(block, var, result);
                // create_pred_upsilons may eliminate the phi and return a different value
                self.create_pred_upsilons(block, phi)
            }
        };

        self.write_variable(block, var, result);
        result
    }
}

impl Default for IrBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_irgen() {
        let mut builder = IrBuilder::new();

        // Create a simple block with some instructions
        let entry_block = builder.create_block();
        builder.seal_block(entry_block);
        builder.label(entry_block);

        // Create a simple computation: return 5 + 5
        let const5 = Instr::const_i32(5);
        let add_result = builder.add(const5, const5);
        builder.ret(add_result);

        // Verify we have created the expected number of blocks
        assert_eq!(builder.blocks.len(), 1); // RefMap counts user blocks (entry_block)
    }

    #[test]
    fn test_variable_operations() {
        let mut builder = IrBuilder::new();

        // Create variable and blocks
        let var = builder.create_variable(TypeId::i32_id());
        let block = builder.create_block();
        builder.seal_block(block);

        // Write and read variable
        let val = Instr::const_i32(42);
        builder.write_variable(block, var, val);
        let read_val = builder.read_variable(block, var);

        // Should get back the same value
        match read_val {
            Instr::ConstI32(_, value) => assert_eq!(value, 42),
            _ => panic!("Expected constant i32"),
        }
    }

    #[test]
    fn test_phi_creation() {
        let mut builder = IrBuilder::new();

        // Create phi node
        let phi = builder.create_phi();
        let phi_instr = builder.phi(phi, TypeId::i32_id());

        // Should create an identity instruction pointing to the phi
        match phi_instr {
            Instr::Identity(meta, _) => {
                assert_eq!(meta.get_type_id(), TypeId::i32_id());
            }
            _ => panic!("Expected identity instruction"),
        }
    }

    #[test]
    fn test_instr_as_hashmap_key() {
        use std::collections::HashMap;

        let mut map: HashMap<Instr, InstrRef> = HashMap::new();

        // Test that instructions can be used as keys
        let instr1 = Instr::const_i32(42);
        let instr2 = Instr::const_bool(true);
        let ref1 = InstrRef::new(1).unwrap();
        let ref2 = InstrRef::new(2).unwrap();

        map.insert(instr1, ref1);
        map.insert(instr2, ref2);

        // Test lookup
        assert_eq!(map.get(&instr1), Some(&ref1));
        assert_eq!(map.get(&instr2), Some(&ref2));

        // Test that equal instructions map to same key
        let instr1_copy = Instr::const_i32(42);
        assert_eq!(map.get(&instr1_copy), Some(&ref1));
    }

    #[test]
    fn test_code_invariants() {
        let builder = IrBuilder::new();

        // Verify index 0 contains Nop
        match builder.code[0] {
            Instr::Nop(_) => (),
            _ => panic!("Index 0 should always contain Nop"),
        }

        // Test that emitted instructions get valid refs
        let mut builder = IrBuilder::new();
        let entry_block = builder.create_block();
        builder.seal_block(entry_block);
        builder.label(entry_block);

        // Emit some instructions
        let const_instr = Instr::const_i32(42);
        let add_result = builder.add(const_instr, const_instr);
        builder.ret(add_result);

        // Verify we have user instructions (more than 0, since Nop doesn't count)
        assert!(builder.code.pinned_count() > 0);

        // Verify we can access the Nop safely
        assert_eq!(builder.code[0].get_type_id(), TypeId::unit_id());
    }

    #[test]
    fn test_add_zero_optimization() {
        let mut builder = IrBuilder::new();

        let zero = Instr::const_i32(0);
        let two = Instr::const_i32(2);
        let three = Instr::const_i32(3);
        let arg = builder.arg(0, TypeId::i32_id());

        let result1 = builder.add(zero, arg);
        match result1 {
            Instr::Arg(_, arg_idx) => assert_eq!(arg_idx, 0),
            _ => panic!("Expected arg instruction, got {:?}", result1),
        }

        let result2 = builder.add(arg, zero);
        match result2 {
            Instr::Arg(_, arg_idx) => assert_eq!(arg_idx, 0),
            _ => panic!("Expected arg instruction, got {:?}", result2),
        }

        let result3 = builder.add(two, three);
        match result3 {
            Instr::ConstI32(_, val) => assert_eq!(val, 5),
            _ => panic!("Expected constant 5, got {:?}", result3),
        }
    }

    #[test]
    fn test_block_ref_type_safety() {
        let mut builder = IrBuilder::new();

        // Create two blocks
        let block1 = builder.create_block();
        let block2 = builder.create_block();

        // Seal and label first block
        builder.seal_block(block1);
        builder.label(block1);

        // Jump to second block - this should populate predecessors correctly
        builder.jump(block2);
        builder.seal_block(block2);

        // Verify that block2's preds contains block1
        let block2_bb = builder.blocks.get(block2);
        assert_eq!(block2_bb.preds.len(), 1);
        assert_eq!(block2_bb.preds[0], block1);

        // Test that we can read the BlockRef values without conversion
        for pred_block in &block2_bb.preds {
            assert!(pred_block.get() > 0); // Should be a valid block reference
        }
    }

    #[test]
    fn test_binding_key_sizes() {
        // Test that tuple key has same size as u32 key
        let tuple_size = std::mem::size_of::<(BlockRef, VarRef)>();
        let u32_size = std::mem::size_of::<u32>();

        println!("Tuple (BlockRef, VarRef) size: {} bytes", tuple_size);
        println!("u32 size: {} bytes", u32_size);

        assert_eq!(
            tuple_size, u32_size,
            "Tuple key should be same size as u32 key"
        );

        // Also verify individual ref sizes
        assert_eq!(std::mem::size_of::<BlockRef>(), 2);
        assert_eq!(std::mem::size_of::<VarRef>(), 2);
    }

    #[test]
    fn test_iterative_phi_elimination() {
        // Test that phi elimination works through chains of identities
        let mut builder = IrBuilder::new();

        // Create a simple chain where phi elimination should cascade
        let var = builder.create_variable(TypeId::i32_id());
        let block1 = builder.create_block();
        let block2 = builder.create_block();
        let block3 = builder.create_block();

        // Set up block1 with a constant
        builder.seal_block(block1);
        builder.label(block1);
        builder.write_variable(block1, var, Instr::const_i32(42));
        builder.jump(block3);

        // Set up block2 with same constant
        builder.seal_block(block2);
        builder.label(block2);
        builder.write_variable(block2, var, Instr::const_i32(42));
        builder.jump(block3);

        // block3 should create a phi that gets eliminated
        builder.label(block3);
        builder.seal_block(block3); // Seal first so phi processing happens immediately
        let phi_result = builder.read_variable(block3, var); // Now read the optimized result

        // The phi should be eliminated and read_variable should return the replacement value directly
        // Since both predecessors provide ConstI32(42), phi elimination should return ConstI32(42)
        match phi_result {
            Instr::ConstI32(_, val) => assert_eq!(val, 42),
            _ => panic!(
                "Expected ConstI32(42) after phi elimination, got {:?}",
                phi_result
            ),
        }
    }

    #[test]
    #[should_panic(expected = "Reading undefined variable")]
    fn test_zero_predecessor_handling() {
        let mut builder = IrBuilder::new();

        // Create entry block with no predecessors
        let entry_block = builder.create_block();
        let var = builder.create_variable(TypeId::i32_id());

        builder.seal_block(entry_block);
        builder.label(entry_block);

        // This should panic since we're reading an undefined variable in entry block
        builder.read_variable(entry_block, var);
    }

    #[test]
    fn test_phi_upsilons_type_safety() {
        let mut builder = IrBuilder::new();

        // Create variable and blocks for phi creation
        let var = builder.create_variable(TypeId::i32_id());
        let block1 = builder.create_block();
        let block2 = builder.create_block();
        let block3 = builder.create_block();

        // Set up a scenario that will create phi nodes
        builder.seal_block(block1);
        builder.label(block1);
        builder.write_variable(block1, var, Instr::const_i32(10));
        builder.jump(block3);

        builder.seal_block(block2);
        builder.label(block2);
        builder.write_variable(block2, var, Instr::const_i32(20));
        builder.jump(block3);

        // Create phi in block3 (multiple predecessors)
        builder.label(block3);
        let _phi_val = builder.read_variable(block3, var);
        builder.seal_block(block3);

        // We've successfully created and accessed phi nodes through the RefMap API
        // The fact that we got here without panicking means the RefMap is working correctly
    }

    #[test]
    fn test_all_new_operators() {
        let mut builder = IrBuilder::new();

        // Test arithmetic operators
        let two = Instr::const_i32(2);
        let three = Instr::const_i32(3);
        let zero = Instr::const_i32(0);
        let one = Instr::const_i32(1);

        // Test subtraction with optimizations
        let sub_result = builder.sub(three, two);
        match sub_result {
            Instr::ConstI32(_, val) => assert_eq!(val, 1),
            _ => panic!("Expected constant folding for subtraction"),
        }

        let sub_zero = builder.sub(three, zero);
        match sub_zero {
            Instr::ConstI32(_, val) => assert_eq!(val, 3),
            _ => panic!("Expected x - 0 = x optimization"),
        }

        // Test multiplication with optimizations
        let mul_result = builder.mul(two, three);
        match mul_result {
            Instr::ConstI32(_, val) => assert_eq!(val, 6),
            _ => panic!("Expected constant folding for multiplication"),
        }

        let mul_zero = builder.mul(three, zero);
        match mul_zero {
            Instr::ConstI32(_, val) => assert_eq!(val, 0),
            _ => panic!("Expected x * 0 = 0 optimization"),
        }

        let mul_one = builder.mul(three, one);
        match mul_one {
            Instr::ConstI32(_, val) => assert_eq!(val, 3),
            _ => panic!("Expected x * 1 = x optimization"),
        }

        // Test division with optimizations
        let div_result = builder.div(Instr::const_i32(6), two);
        match div_result {
            Instr::ConstI32(_, val) => assert_eq!(val, 3),
            _ => panic!("Expected constant folding for division"),
        }

        let div_one = builder.div(three, one);
        match div_one {
            Instr::ConstI32(_, val) => assert_eq!(val, 3),
            _ => panic!("Expected x / 1 = x optimization"),
        }

        // Test comparison operators
        let lt_result = builder.lt(two, three);
        match lt_result {
            Instr::ConstBool(_, val) => assert_eq!(val, true),
            _ => panic!("Expected constant folding for less than"),
        }

        let gt_result = builder.gt(three, two);
        match gt_result {
            Instr::ConstBool(_, val) => assert_eq!(val, true),
            _ => panic!("Expected constant folding for greater than"),
        }

        let lt_eq_result = builder.lt_eq(two, two);
        match lt_eq_result {
            Instr::ConstBool(_, val) => assert_eq!(val, true),
            _ => panic!("Expected constant folding for less than or equal"),
        }

        let gt_eq_result = builder.gt_eq(three, three);
        match gt_eq_result {
            Instr::ConstBool(_, val) => assert_eq!(val, true),
            _ => panic!("Expected constant folding for greater than or equal"),
        }

        // Test logical operators with short-circuit optimizations
        let true_val = Instr::const_bool(true);
        let false_val = Instr::const_bool(false);

        let and_result = builder.and(true_val, false_val);
        match and_result {
            Instr::ConstBool(_, val) => assert_eq!(val, false),
            _ => panic!("Expected constant folding for AND"),
        }

        let and_true = builder.and(true_val, true_val);
        match and_true {
            Instr::ConstBool(_, val) => assert_eq!(val, true),
            _ => panic!("Expected true && true = true optimization"),
        }

        let or_result = builder.or(false_val, true_val);
        match or_result {
            Instr::ConstBool(_, val) => assert_eq!(val, true),
            _ => panic!("Expected constant folding for OR"),
        }

        let or_false = builder.or(false_val, false_val);
        match or_false {
            Instr::ConstBool(_, val) => assert_eq!(val, false),
            _ => panic!("Expected false || false = false optimization"),
        }

        // Test unary operators
        let neg_result = builder.neg(three);
        match neg_result {
            Instr::ConstI32(_, val) => assert_eq!(val, -3),
            _ => panic!("Expected constant folding for negation"),
        }

        let not_result = builder.not(true_val);
        match not_result {
            Instr::ConstBool(_, val) => assert_eq!(val, false),
            _ => panic!("Expected constant folding for logical NOT"),
        }
    }
}
