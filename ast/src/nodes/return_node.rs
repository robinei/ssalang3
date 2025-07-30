use ir::Instr;

use crate::{AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct ReturnNode {
    pub value_node: NodeHandle,
}

impl AstNode for ReturnNode {
    const NODE_TYPE: NodeType = NodeType::Return;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        if !context.func.is_in_body {
            return Err(CompileError::Error(
                Some(handle.untyped()),
                "can only return in function bodies".to_string(),
            ));
        }

        let return_value = self.value_node.compile(context)?;

        if let Some(return_type) = context.func.return_type {
            if return_value.get_type_id() != return_type {
                return Err(CompileError::type_error(
                    Some(self.value_node),
                    return_type,
                    return_value.get_type_id(),
                    "return statement".to_string(),
                ));
            }
        } else {
            context.func.return_type = Some(return_value.get_type_id());
        }

        if context.static_eval {
            return Err(CompileError::Return(return_value));
        }

        context.func.irbuilder.emit_ret(return_value);
        Ok(Instr::never())
    }
}
