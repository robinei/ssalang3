use std::{marker::PhantomData, num::NonZeroU32};

use crate::{NodeType, nodes::*};

mod ast_arena;
mod ast_node;
mod node_type;
pub mod nodes;

pub use ast_arena::*;
pub use ast_node::*;
use common::{Symbol, TypeId};
use ir::{Instr, IrBuilder};
pub use node_type::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Value {
    pub instr: Instr,
}

impl Value {
    #[inline]
    pub fn new(instr: Instr) -> Self {
        Self { instr }
    }

    #[inline]
    pub fn unit() -> Self {
        Self::new(Instr::nop())
    }

    #[inline]
    pub fn get_type_id(&self) -> TypeId {
        self.instr.get_type_id()
    }
}

pub enum CompileError {
    Return(Value),
    Break(Option<Symbol>, Value),
    Continue(Option<Symbol>),
    Error(Option<NodeHandle>, String),
}

impl CompileError {
    fn type_error(
        node: Option<NodeHandle>,
        expected: TypeId,
        found: TypeId,
        errcontext: String,
    ) -> CompileError {
        CompileError::Error(
            node,
            format!(
                "expected type {}, but found type {} in {}",
                expected, found, errcontext
            ),
        )
    }
}

type CompileResult = Result<Value, CompileError>;

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
    #[inline]
    pub fn node_type(self) -> NodeType {
        // Safety: We control construction and NodeType has valid discriminants 1-21
        unsafe { std::mem::transmute((self.handle.get() & 0xFF) as u8) }
    }

    #[inline]
    pub fn byte_offset(self) -> u32 {
        (self.handle.get() >> 8) * 4
    }

    // Convert to strongly typed handle if types match
    #[inline]
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

    #[inline]
    fn comp<T: AstNode>(self, context: &mut CompileContext) -> CompileResult {
        self.typed::<T>().unwrap().compile(context)
    }

    pub fn compile(self, context: &mut CompileContext) -> CompileResult {
        match self.node_type() {
            NodeType::Assign => self.comp::<AssignNode>(context),
            NodeType::Binop => self.comp::<BinopNode>(context),
            NodeType::Block => self.comp::<BlockNode>(context),
            NodeType::Borrow => self.comp::<BorrowNode>(context),
            NodeType::Break => self.comp::<BreakNode>(context),
            NodeType::ConstUnit => self.comp::<ConstUnitNode>(context),
            NodeType::ConstBool => self.comp::<ConstBoolNode>(context),
            NodeType::ConstI32 => self.comp::<ConstI32Node>(context),
            NodeType::ConstString => self.comp::<ConstStringNode>(context),
            NodeType::Continue => self.comp::<ContinueNode>(context),
            NodeType::Fn => self.comp::<FnNode>(context),
            NodeType::Ident => self.comp::<IdentNode>(context),
            NodeType::If => self.comp::<IfNode>(context),
            NodeType::Let => self.comp::<LetNode>(context),
            NodeType::LetFn => self.comp::<LetFnNode>(context),
            NodeType::Module => self.comp::<ModuleNode>(context),
            NodeType::Return => self.comp::<ReturnNode>(context),
            NodeType::Struct => self.comp::<StructNode>(context),
            NodeType::TypeAtom => self.comp::<TypeAtomNode>(context),
            NodeType::Unop => self.comp::<UnopNode>(context),
            NodeType::While => self.comp::<WhileNode>(context),
        }
    }
}

impl<T: AstNode> From<TypedNodeHandle<T>> for NodeHandle {
    #[inline]
    fn from(typed: TypedNodeHandle<T>) -> Self {
        typed.untyped()
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
    #[inline]
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
    #[inline]
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

    #[inline]
    pub fn node_type(self) -> NodeType {
        T::NODE_TYPE
    }

    #[inline]
    pub fn byte_offset(self) -> u32 {
        (self.handle.get() >> 8) * 4
    }

    // Convert to type-erased handle
    #[inline]
    pub fn untyped(self) -> NodeHandle {
        NodeHandle {
            handle: self.handle,
        }
    }

    #[inline]
    pub fn compile(self, context: &mut CompileContext) -> CompileResult {
        let result = context.ast.get(self).compile(context, self)?;
        if context.static_eval && !result.instr.is_const() {
            return Err(CompileError::Error(
                Some(self.untyped()),
                "expected const value".to_string(),
            ));
        }
        Ok(result)
    }
}

pub struct CompileContext<'a> {
    pub ast: &'a AstArena,
    pub irbuilder: &'a mut IrBuilder,
    static_eval: bool,
}

impl<'a> CompileContext<'a> {
    #[inline]
    fn with_static_eval<R, F: FnOnce(&mut Self) -> R>(&mut self, static_eval: bool, f: F) -> R {
        let prev = self.static_eval;
        self.static_eval = self.static_eval || static_eval;
        let result = f(self);
        self.static_eval = prev;
        result
    }
}
