use std::{marker::PhantomData, num::NonZeroU32};

use crate::NodeType;

pub trait ArrayLen: Default + Copy {
    fn create_len(value: u32) -> Self;
    fn get_value(self) -> u32;
}

impl ArrayLen for u32 {
    fn create_len(value: u32) -> Self {
        value
    }

    fn get_value(self) -> u32 {
        self
    }
}

impl ArrayLen for () {
    fn create_len(_: u32) -> Self {
        ()
    }

    fn get_value(self) -> u32 {
        0
    }
}

// Trait that all node types must implement
pub trait AstNode: Sized {
    const NODE_TYPE: NodeType;
    type LengthType: ArrayLen;
    type ElementType;

    fn compile(&self, context: &mut CompileContext);
}

// Type-erased handle that encodes both node type and offset
// Uses niche optimization: node type 0 is reserved as sentinel for None in Option<NodeHandle>
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeHandle {
    handle: NonZeroU32,
}

impl std::fmt::Debug for NodeHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NodeHandle")
            .field("type", &self.node_type())
            .field("offset", &self.byte_offset())
            .finish()
    }
}

impl NodeHandle {
    pub fn node_type(self) -> NodeType {
        // Safety: We control construction and NodeType has valid discriminants 1-21
        unsafe { std::mem::transmute((self.handle.get() & 0xFF) as u8) }
    }

    pub fn byte_offset(self) -> u32 {
        (self.handle.get() >> 8) * 4
    }

    // Convert to strongly typed handle if types match
    pub fn typed<T: AstNode>(self) -> Option<TypedNodeHandle<T>> {
        if self.node_type() == T::NODE_TYPE {
            Some(TypedNodeHandle::<T> {
                handle: self.handle,
                _phantom: PhantomData,
            })
        } else {
            None
        }
    }

    pub fn compile(self, context: &mut CompileContext) {
        match self.node_type() {
            NodeType::Assign => context
                .ast
                .get(self.typed::<nodes::AssignNode>().unwrap())
                .compile(context),
            NodeType::Binop => context
                .ast
                .get(self.typed::<nodes::BinopNode>().unwrap())
                .compile(context),
            NodeType::Block => context
                .ast
                .get(self.typed::<nodes::BlockNode>().unwrap())
                .compile(context),
            NodeType::Borrow => context
                .ast
                .get(self.typed::<nodes::BorrowNode>().unwrap())
                .compile(context),
            NodeType::Break => context
                .ast
                .get(self.typed::<nodes::BreakNode>().unwrap())
                .compile(context),
            NodeType::ConstUnit => context
                .ast
                .get(self.typed::<nodes::ConstUnitNode>().unwrap())
                .compile(context),
            NodeType::ConstBool => context
                .ast
                .get(self.typed::<nodes::ConstBoolNode>().unwrap())
                .compile(context),
            NodeType::ConstI32 => context
                .ast
                .get(self.typed::<nodes::ConstI32Node>().unwrap())
                .compile(context),
            NodeType::ConstString => context
                .ast
                .get(self.typed::<nodes::ConstStringNode>().unwrap())
                .compile(context),
            NodeType::Continue => context
                .ast
                .get(self.typed::<nodes::ContinueNode>().unwrap())
                .compile(context),
            NodeType::Fn => context
                .ast
                .get(self.typed::<nodes::FnNode>().unwrap())
                .compile(context),
            NodeType::Ident => context
                .ast
                .get(self.typed::<nodes::IdentNode>().unwrap())
                .compile(context),
            NodeType::If => context
                .ast
                .get(self.typed::<nodes::IfNode>().unwrap())
                .compile(context),
            NodeType::Let => context
                .ast
                .get(self.typed::<nodes::LetNode>().unwrap())
                .compile(context),
            NodeType::LetFn => context
                .ast
                .get(self.typed::<nodes::LetFnNode>().unwrap())
                .compile(context),
            NodeType::Module => context
                .ast
                .get(self.typed::<nodes::ModuleNode>().unwrap())
                .compile(context),
            NodeType::Return => context
                .ast
                .get(self.typed::<nodes::ReturnNode>().unwrap())
                .compile(context),
            NodeType::Struct => context
                .ast
                .get(self.typed::<nodes::StructNode>().unwrap())
                .compile(context),
            NodeType::TypeAtom => context
                .ast
                .get(self.typed::<nodes::TypeAtomNode>().unwrap())
                .compile(context),
            NodeType::Unop => context
                .ast
                .get(self.typed::<nodes::UnopNode>().unwrap())
                .compile(context),
            NodeType::While => context
                .ast
                .get(self.typed::<nodes::WhileNode>().unwrap())
                .compile(context),
        }
    }
}

// Strongly-typed handle that knows its node type at compile time
// Uses niche optimization: node type 0 is reserved for None in Option<TypedNodeHandle>
#[derive(PartialEq, Eq)]
#[repr(transparent)]
pub struct TypedNodeHandle<T: AstNode> {
    handle: NonZeroU32,
    _phantom: PhantomData<T>,
}

// manually implement these because the derive macro is confused and refuses to impl them because of the T (which does not have these bounds) in PhantomData
impl<T: AstNode> Copy for TypedNodeHandle<T> {}
impl<T: AstNode> Clone for TypedNodeHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Manual Debug implementation to avoid PhantomData noise
impl<T: AstNode> std::fmt::Debug for TypedNodeHandle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypedNodeHandle")
            .field("type", &T::NODE_TYPE)
            .field("offset", &self.byte_offset())
            .finish()
    }
}

impl<T: AstNode> TypedNodeHandle<T> {
    fn new(byte_offset: u32) -> Self {
        debug_assert!(byte_offset % 4 == 0, "Byte offset must be 4-byte aligned");
        let offset_div4 = byte_offset / 4;
        debug_assert!(
            offset_div4 < (1 << 24),
            "Offset too large for 24-bit handle"
        );
        let handle_bits = (T::NODE_TYPE as u32) | (offset_div4 << 8);
        debug_assert!(
            handle_bits != 0,
            "Handle should never be zero (node type 0 reserved for niche)"
        );
        Self {
            handle: NonZeroU32::new(handle_bits).expect("Handle should never be zero"),
            _phantom: PhantomData,
        }
    }

    pub fn node_type(self) -> NodeType {
        T::NODE_TYPE
    }

    pub fn byte_offset(self) -> u32 {
        (self.handle.get() >> 8) * 4
    }

    // Convert to type-erased handle
    pub fn untyped(self) -> NodeHandle {
        NodeHandle {
            handle: self.handle,
        }
    }

    pub fn compile(self, context: &mut CompileContext) {
        context.ast.get(self).compile(context)
    }
}

impl<T: AstNode> From<TypedNodeHandle<T>> for NodeHandle {
    fn from(typed: TypedNodeHandle<T>) -> Self {
        typed.untyped()
    }
}

pub struct CompileContext<'a> {
    ast: &'a AstArena,
}

mod ast_arena;
mod node_type;
pub mod nodes;

pub use ast_arena::*;
pub use node_type::*;
