use crate::{AstNode, CompileContext, NodeHandle, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinopType {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Eq,     // ==
    Neq,    // !=
    Lt,     // <
    Gt,     // >
    LtEq,   // <=
    GtEq,   // >=
    And,    // &&
    Or,     // ||
    BitAnd, // &
    BitOr,  // |
}

pub struct BinopNode {
    pub op_type: BinopType,
    pub left_node: NodeHandle,
    pub right_node: NodeHandle,
}

impl AstNode for BinopNode {
    const NODE_TYPE: NodeType = NodeType::Binop;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
