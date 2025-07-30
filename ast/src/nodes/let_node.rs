use common::Symbol;
use ir::Instr;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct LetNode {
    pub name: Symbol,
    pub is_mutable: bool,
    pub type_node: Option<NodeHandle>,
    pub value_node: Option<NodeHandle>,
}

impl AstNode for LetNode {
    const NODE_TYPE: NodeType = NodeType::Let;
    type LengthType = ();
    type ElementType = ();

    #[inline]
    fn compile(&self, context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        self.compile_let(context)
    }
}

impl LetNode {
    pub fn compile_let(&self, context: &mut CompileContext) -> CompileResult {
        let type_id = match self.type_node {
            Some(type_node) => Some(context.compile_type(type_node)?),
            None => None,
        };

        let value = match self.value_node {
            Some(value_node) => Some(value_node.compile(context)?),
            None => None,
        };

        if let (Some(type_id), Some(value)) = (type_id, &value) {
            if value.get_type_id() != type_id {
                return Err(crate::CompileError::type_error(
                    self.value_node,
                    type_id,
                    value.get_type_id(),
                    "let value".to_string(),
                ));
            }
        }

        context.env.push_slot(self.name, self.is_mutable, type_id, value);
        Ok(Instr::const_unit())
    }
}
