use common::Symbol;

use crate::{AstNode, CompileContext, NodeHandle, NodeType};

pub struct AssignNode {
    pub name: Symbol,
    pub value_node: NodeHandle,
}

impl AstNode for AssignNode {
    const NODE_TYPE: NodeType = NodeType::Assign;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
