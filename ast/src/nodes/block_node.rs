use crate::{AstNode, CompileContext, NodeHandle, NodeType};

pub struct BlockNode {}

impl AstNode for BlockNode {
    const NODE_TYPE: NodeType = NodeType::Block;
    type LengthType = u32;
    type ElementType = NodeHandle;

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
