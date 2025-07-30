use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeType, TypedNodeHandle};

pub struct IdentNode {
    pub name: Symbol,
}

impl AstNode for IdentNode {
    const NODE_TYPE: NodeType = NodeType::Ident;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        let Some(slot_index) = context.env.get_slot_index(self.name) else {
            return Err(crate::CompileError::Error(Some(handle.untyped()), "unknown symbol".to_string()));
        };

        let slot = &mut context.env.slots[slot_index];
        let copyable = true; // TODO
        let value = if copyable {
            slot.value.clone()
        } else {
            std::mem::take(&mut slot.value)
        };
        let Some(value) = value else {
            return Err(crate::CompileError::Error(Some(handle.untyped()), "undefined symbol".to_string()));
        };

        Ok(value)
    }
}
