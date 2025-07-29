use crate::{AstNode, CompileContext, NodeHandle, NodeType, TypedNodeHandle, nodes::BlockNode};

pub struct WhileNode {
    pub is_inline: bool,
    pub cond_node: NodeHandle,
    pub body_node: TypedNodeHandle<BlockNode>,
}

impl AstNode for WhileNode {
    const NODE_TYPE: NodeType = NodeType::While;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
