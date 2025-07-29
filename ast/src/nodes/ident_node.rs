use common::Symbol;

use crate::{AstNode, CompileContext, NodeType};

pub struct IdentNode {
    pub name: Symbol,
}

impl AstNode for IdentNode {
    const NODE_TYPE: NodeType = NodeType::Ident;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
