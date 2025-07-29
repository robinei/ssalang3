use crate::{AstNode, CompileContext, NodeHandle, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnopType {
    Neg,    // -
    Not,    // !
    BitNot, // ~
}

pub struct UnopNode {
    pub op_type: UnopType,
    pub operand_node: NodeHandle,
}

impl AstNode for UnopNode {
    const NODE_TYPE: NodeType = NodeType::Unop;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
