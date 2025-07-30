use common::Symbol;
use ir::Instr;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct AssignNode {
    pub name: Symbol,
    pub value_node: NodeHandle,
}

impl AstNode for AssignNode {
    const NODE_TYPE: NodeType = NodeType::Assign;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        let Some(slot_index) = context.env.get_slot_index(self.name) else {
            return Err(crate::CompileError::Error(Some(handle.untyped()), "unknown symbol".to_string()));
        };

        let value = self.value_node.compile(context)?;

        let slot = &mut context.env.slots[slot_index];

        if let Some(type_id) = slot.type_id {
            if value.get_type_id() != type_id {
                return Err(crate::CompileError::type_error(
                    Some(self.value_node),
                    type_id,
                    value.get_type_id(),
                    "assignment".to_string(),
                ));
            }
        } else {
            slot.type_id = Some(value.get_type_id());
        }

        if !slot.is_mutable && slot.value != None {
            return Err(crate::CompileError::Error(
                Some(handle.untyped()),
                "cannot reassign immutable binding".to_string(),
            ));
        }

        slot.value = Some(value);

        Ok(Instr::const_unit())
    }
}
