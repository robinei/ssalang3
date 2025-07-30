use common::Symbol;

use crate::{
    AstNode, CompileContext, CompileResult, NodeType, TypedNodeHandle,
    nodes::{FnNode, LetNode},
};

pub struct LetFnNode {
    pub name: Symbol,
    pub fn_node: TypedNodeHandle<FnNode>,
}

impl AstNode for LetFnNode {
    const NODE_TYPE: NodeType = NodeType::LetFn;
    type LengthType = ();
    type ElementType = ();

    fn compile(
        &self,
        context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        LetNode {
            name: self.name,
            is_mutable: false,
            type_node: None,
            value_node: Some(self.fn_node.untyped()),
        }
        .compile_let(context)
    }
}
