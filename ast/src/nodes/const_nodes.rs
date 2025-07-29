use crate::{AstArena, AstNode, CompileContext, NodeType, TypedNodeHandle};

pub struct ConstUnitNode {}

impl AstNode for ConstUnitNode {
    const NODE_TYPE: NodeType = NodeType::ConstUnit;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}

pub struct ConstBoolNode {
    pub value: bool,
}

impl AstNode for ConstBoolNode {
    const NODE_TYPE: NodeType = NodeType::ConstBool;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}

pub struct ConstI32Node {
    pub value: i32,
}

impl AstNode for ConstI32Node {
    const NODE_TYPE: NodeType = NodeType::ConstI32;
    type LengthType = ();
    type ElementType = ();

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}

pub struct ConstStringNode {}

impl AstNode for ConstStringNode {
    const NODE_TYPE: NodeType = NodeType::ConstString;
    type LengthType = u32;
    type ElementType = u8;

    fn compile(&self, _context: &mut CompileContext) {
        todo!()
    }
}

impl ConstStringNode {
    pub fn get_string<'a>(
        arena: &'a AstArena,
        handle: TypedNodeHandle<ConstStringNode>,
    ) -> &'a str {
        let bytes = arena.get_array(handle);
        // This is allocation-free - just creates a &str view over existing bytes
        std::str::from_utf8(bytes).expect("Invalid UTF-8 in string node")
    }
}
