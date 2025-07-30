use ir::IrBuilderError;

use crate::{AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinopType {
    Add,     // +
    Sub,     // -
    Mul,     // *
    Div,     // /
    Eq,      // ==
    Neq,     // !=
    Lt,      // <
    Gt,      // >
    LtEq,    // <=
    GtEq,    // >=
    LogiAnd, // &&
    LogiOr,  // ||
             //BitAnd, // &
             //BitOr,  // |
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

    fn compile(&self, context: &mut CompileContext, handle: TypedNodeHandle<Self>) -> CompileResult {
        let left = self.left_node.compile(context)?;
        let right = self.right_node.compile(context)?;

        match self.op_type {
            BinopType::Add => context.func.irbuilder.emit_add(left, right),
            BinopType::Sub => context.func.irbuilder.emit_sub(left, right),
            BinopType::Mul => context.func.irbuilder.emit_mul(left, right),
            BinopType::Div => context.func.irbuilder.emit_div(left, right),
            BinopType::Eq => context.func.irbuilder.emit_eq(left, right),
            BinopType::Neq => context.func.irbuilder.emit_neq(left, right),
            BinopType::Lt => context.func.irbuilder.emit_lt(left, right),
            BinopType::Gt => context.func.irbuilder.emit_gt(left, right),
            BinopType::LtEq => context.func.irbuilder.emit_lt_eq(left, right),
            BinopType::GtEq => context.func.irbuilder.emit_gt_eq(left, right),
            BinopType::LogiAnd => context.func.irbuilder.emit_logi_and(left, right),
            BinopType::LogiOr => context.func.irbuilder.emit_logi_or(left, right),
        }
        .map_err(|err: IrBuilderError| {
            CompileError::Error(
                Some(match err.component {
                    0 => handle.untyped(),
                    1 => self.left_node,
                    2 => self.right_node,
                    _ => unreachable!(),
                }),
                err.message,
            )
        })
    }
}
