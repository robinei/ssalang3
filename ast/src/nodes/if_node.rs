use crate::{AstNode, CompileContext, NodeHandle, NodeType, TypedNodeHandle, nodes::BlockNode};

pub struct IfNode {
    pub is_inline: bool,
    pub cond_node: NodeHandle,
    pub then_node: TypedNodeHandle<BlockNode>,
    pub else_node: NodeHandle,
}

impl AstNode for IfNode {
    const NODE_TYPE: NodeType = NodeType::If;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
