use common::TypeId;
use ir::Instr;

use crate::{AstArena, AstNode, CompileContext, CompileError, CompileResult, NodeType, TypedNodeHandle};

pub struct ConstUnitNode {}

impl AstNode for ConstUnitNode {
    const NODE_TYPE: NodeType = NodeType::ConstUnit;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Ok(Instr::const_unit())
    }
}

pub struct ConstBoolNode {
    pub value: bool,
}

impl AstNode for ConstBoolNode {
    const NODE_TYPE: NodeType = NodeType::ConstBool;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Ok(Instr::const_bool(self.value))
    }
}

pub struct ConstIntNode {
    pub value: i64,
}

impl AstNode for ConstIntNode {
    const NODE_TYPE: NodeType = NodeType::ConstInt;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Ok(Instr::const_int(self.value))
    }
}

pub struct ConstUintNode {
    pub value: u64,
}

impl AstNode for ConstUintNode {
    const NODE_TYPE: NodeType = NodeType::ConstUint;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Ok(Instr::const_uint(self.value))
    }
}

pub struct ConstFloatNode {
    pub value: f64,
}

impl AstNode for ConstFloatNode {
    const NODE_TYPE: NodeType = NodeType::ConstFloat;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Ok(Instr::const_float(self.value))
    }
}

pub struct ConstStringNode {}

impl AstNode for ConstStringNode {
    const NODE_TYPE: NodeType = NodeType::ConstString;
    type LengthType = u32;
    type ElementType = u8;

    fn compile(&self, context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Err(CompileError::Error(self.get_untyped_handle(context), "not implemented".to_string()))
    }
}

impl ConstStringNode {
    pub fn get_string<'a>(arena: &'a AstArena, handle: TypedNodeHandle<ConstStringNode>) -> &'a str {
        let bytes = arena.get_array(handle);
        // This is allocation-free - just creates a &str view over existing bytes
        std::str::from_utf8(bytes).expect("Invalid UTF-8 in string node")
    }
}

pub struct ConstTypeIdNode {
    pub type_id: TypeId,
}

impl AstNode for ConstTypeIdNode {
    const NODE_TYPE: NodeType = NodeType::ConstTypeId;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext, _handle: TypedNodeHandle<Self>) -> CompileResult {
        Ok(Instr::const_type(self.type_id))
    }
}
