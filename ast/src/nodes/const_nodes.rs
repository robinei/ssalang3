use macros::ast_node;

use crate::{AstArena, AstNode, NodeType, TypedNodeHandle};

#[ast_node(NodeType::ConstUnit)]
pub struct ConstUnitNode {}

#[ast_node(NodeType::ConstBool)]
pub struct ConstBoolNode {
    pub value: bool,
}

#[ast_node(NodeType::ConstI32)]
pub struct ConstI32Node {
    pub value: i32,
}

#[ast_node(NodeType::ConstString, array = u8)]
pub struct ConstStringNode {}

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
