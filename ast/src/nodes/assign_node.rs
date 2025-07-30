use common::Symbol;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct AssignNode {
    pub name: Symbol,
    pub value_node: NodeHandle,
}

impl AstNode for AssignNode {
    const NODE_TYPE: NodeType = NodeType::Assign;
    type LengthType = ();
    type ElementType = ();

    fn compile(
        &self,
        _context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        todo!()
    }
}
