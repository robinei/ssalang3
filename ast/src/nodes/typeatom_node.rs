use crate::{AstNode, CompileContext, NodeType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeAtom {
    Unit,
    Bool,
    I32,
    String,
}

pub struct TypeAtomNode {
    pub atom: TypeAtom,
}

impl AstNode for TypeAtomNode {
    const NODE_TYPE: NodeType = NodeType::TypeAtom;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}
