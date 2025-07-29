use crate::{AstNode, CompileContext, NodeHandle, NodeType};

pub struct ReturnNode {
    pub value_node: NodeHandle,
}

impl AstNode for ReturnNode {
    const NODE_TYPE: NodeType = NodeType::Return;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
