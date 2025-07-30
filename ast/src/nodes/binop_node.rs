use common::TypeId;

use crate::{
    AstNode, CompileContext, CompileError, CompileResult, NodeHandle, NodeType, TypedNodeHandle,
    Value,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinopType {
    Add,  // +
    Sub,  // -
    Mul,  // *
    Div,  // /
    Eq,   // ==
    Neq,  // !=
    Lt,   // <
    Gt,   // >
    LtEq, // <=
    GtEq, // >=
    And,  // &&
    Or,   // ||
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

    fn compile(
        &self,
        context: &mut CompileContext,
        _handle: TypedNodeHandle<Self>,
    ) -> CompileResult {
        let left = self.left_node.compile(context)?;
        let right = self.right_node.compile(context)?;

        match self.op_type {
            BinopType::Add | BinopType::Sub | BinopType::Mul | BinopType::Div => {
                if left.get_type_id() != TypeId::i32_id() {
                    return Err(CompileError::type_error(
                        Some(self.left_node),
                        TypeId::i32_id(),
                        left.get_type_id(),
                        format!("arithmetic operation {:?}", self.op_type),
                    ));
                }
                if right.get_type_id() != TypeId::i32_id() {
                    return Err(CompileError::type_error(
                        Some(self.right_node),
                        TypeId::i32_id(),
                        right.get_type_id(),
                        format!("arithmetic operation {:?}", self.op_type),
                    ));
                }
                let result_instr = match self.op_type {
                    BinopType::Add => context.irbuilder.emit_add(left.instr, right.instr),
                    BinopType::Sub => context.irbuilder.emit_sub(left.instr, right.instr),
                    BinopType::Mul => context.irbuilder.emit_mul(left.instr, right.instr),
                    BinopType::Div => context.irbuilder.emit_div(left.instr, right.instr),
                    _ => unreachable!(),
                };
                Ok(Value::new(result_instr))
            }

            BinopType::Eq | BinopType::Neq => {
                if left.get_type_id() != right.get_type_id() {
                    return Err(CompileError::type_error(
                        Some(self.right_node),
                        left.get_type_id(),
                        right.get_type_id(),
                        format!("comparison operation {:?}", self.op_type),
                    ));
                }
                let result_instr = match self.op_type {
                    BinopType::Eq => context.irbuilder.emit_eq(left.instr, right.instr),
                    BinopType::Neq => context.irbuilder.emit_neq(left.instr, right.instr),
                    _ => unreachable!(),
                };
                Ok(Value::new(result_instr))
            }

            BinopType::Lt | BinopType::Gt | BinopType::LtEq | BinopType::GtEq => {
                if left.get_type_id() != TypeId::i32_id() {
                    return Err(CompileError::type_error(
                        Some(self.left_node),
                        TypeId::i32_id(),
                        left.get_type_id(),
                        format!("comparison operation {:?}", self.op_type),
                    ));
                }
                if right.get_type_id() != TypeId::i32_id() {
                    return Err(CompileError::type_error(
                        Some(self.right_node),
                        TypeId::i32_id(),
                        right.get_type_id(),
                        format!("comparison operation {:?}", self.op_type),
                    ));
                }
                let result_instr = match self.op_type {
                    BinopType::Lt => context.irbuilder.emit_lt(left.instr, right.instr),
                    BinopType::Gt => context.irbuilder.emit_gt(left.instr, right.instr),
                    BinopType::LtEq => context.irbuilder.emit_lt_eq(left.instr, right.instr),
                    BinopType::GtEq => context.irbuilder.emit_gt_eq(left.instr, right.instr),
                    _ => unreachable!(),
                };
                Ok(Value::new(result_instr))
            }

            BinopType::And | BinopType::Or => {
                if left.get_type_id() != TypeId::bool_id() {
                    return Err(CompileError::type_error(
                        Some(self.left_node),
                        TypeId::bool_id(),
                        left.get_type_id(),
                        format!("logical operation {:?}", self.op_type),
                    ));
                }
                if right.get_type_id() != TypeId::bool_id() {
                    return Err(CompileError::type_error(
                        Some(self.right_node),
                        TypeId::bool_id(),
                        right.get_type_id(),
                        format!("logical operation {:?}", self.op_type),
                    ));
                }
                let result_instr = match self.op_type {
                    BinopType::And => context.irbuilder.emit_logi_and(left.instr, right.instr),
                    BinopType::Or => context.irbuilder.emit_logi_or(left.instr, right.instr),
                    _ => unreachable!(),
                };
                Ok(Value::new(result_instr))
            }
        }
    }
}
