use ir::IrBuilderError;

use crate::{AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnopType {
    Neg, // -
    Not, // !
         //BitNot, // ~
}

pub struct UnopNode {
    pub op_type: UnopType,
    pub operand_node: NodeHandle,
}

impl AstNode for UnopNode {
    const NODE_TYPE: NodeType = NodeType::Unop;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        let operand = self.operand_node.compile(context)?;

        match self.op_type {
            UnopType::Neg => context.func.irbuilder.emit_neg(operand),
            UnopType::Not => context.func.irbuilder.emit_logi_not(operand),
        }
        .map_err(|err: IrBuilderError| {
            CompileError::Error(
                Some(match err.component {
                    0 => handle.untyped(),
                    1 => self.operand_node,
                    _ => unreachable!(),
                }),
                err.message,
            )
        })
    }
}
