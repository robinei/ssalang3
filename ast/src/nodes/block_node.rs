use ir::Instr;

use crate::{AstNode, CompileContext, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

pub struct BlockNode {
    is_static: bool,
}

impl AstNode for BlockNode {
    const NODE_TYPE: NodeType = NodeType::Block;
    type LengthType = u32;
    type ElementType = NodeHandle;

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        context.with_push_scope(|context| {
            context.with_static_eval(self.is_static, |context| {
                let mut last_result = Instr::const_unit();
                for stmt in context.module.ast.get_array(handle) {
                    last_result = stmt.compile(context)?;
                }
                Ok(last_result)
            })
        })
    }
}
