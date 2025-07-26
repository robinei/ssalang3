use std::marker::PhantomData;
use std::mem::{align_of, size_of};
use std::num::NonZeroU32;

use macros::ast_node;

// Core node type enum - uses u8 for compact representation
// NodeType 0 is reserved for niche optimization (enables Option<Handle> same size as Handle)
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeType {
    // Type nodes
    TypeAtom = 1, // Start at 1, reserve 0 for niche optimization

    // Constant nodes
    ConstUnit,
    ConstBool,
    ConstI32,
    ConstString,

    // Operator nodes
    Unop,
    Binop,

    // Local variable nodes
    DefineFn,
    Define,
    Assign,
    LocalRead,
    Borrow,

    // Control flow nodes
    Block,
    While,
    If,

    // Jump nodes
    Break,
    Continue,
    Return,

    // Function node
    Fn,

    // Module node
    Module,

    // Struct nodes
    Struct,
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
}

impl<T: AstNode> From<TypedNodeHandle<T>> for NodeHandle {
    fn from(typed: TypedNodeHandle<T>) -> Self {
        typed.untyped()
    }
}

// NodeInfo struct that embeds source location and node tag
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeInfo {
    // Lower 8 bits: node tag, upper 24 bits: source location
    data: u32,
}

impl NodeInfo {
    pub fn new(node_type: NodeType, source_location: u32) -> Self {
        debug_assert!(
            source_location < (1 << 24),
            "Source location too large for 24-bit storage"
        );
        Self {
            data: (node_type as u32) | (source_location << 8),
        }
    }

    pub fn node_type(&self) -> NodeType {
        // Safety: We control construction and NodeType has valid discriminants 1-21
        unsafe { std::mem::transmute((self.data & 0xFF) as u8) }
    }

    pub fn source_location(&self) -> u32 {
        self.data >> 8
    }
}

// Marker trait for nodes with arrays
pub trait ArrayNode {}

// Trait that all node types must implement
pub trait AstNode: Sized {
    const NODE_TYPE: NodeType;
    type ElementType;
}

// Compile-time check that T doesn't implement Drop
// This will cause a compile error if T implements Drop
const fn assert_no_drop<T>() {
    struct AssertNotDrop<U>(std::marker::PhantomData<U>);
    impl<U> AssertNotDrop<U> {
        const ASSERT: () = {
            // This will fail to compile if T implements Drop
            let _ = std::mem::needs_drop::<U>();
            if std::mem::needs_drop::<U>() {
                panic!("AstNode types cannot implement Drop");
            }
        };
    }
    let _ = AssertNotDrop::<T>::ASSERT;
}

// Internal wrapper types for memory layout
#[repr(C)]
struct NodeWrapper<T: AstNode> {
    node: T,
    info: NodeInfo,
}

#[repr(C)]
struct ArrayNodeWrapper<T: AstNode + ArrayNode> {
    node: T,
    info: NodeInfo,
    array_len: u32,
    // Zero-sized array to get offset of actual array data
    array: [T::ElementType; 0],
}

// Arena allocator for AST nodes
pub struct AstArena {
    buffer: Vec<u8>, // Private - no direct access allowed
}

impl AstArena {
    pub fn new() -> Self {
        Self { buffer: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
        }
    }

    // Read-only access to buffer stats for debugging/monitoring
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn capacity(&self) -> usize {
        self.buffer.capacity()
    }

    // Private wrapper accessor methods
    fn get_node_wrapper<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> &NodeWrapper<T> {
        let byte_offset = handle.byte_offset() as usize;
        assert!(
            byte_offset + size_of::<NodeWrapper<T>>() <= self.buffer.len(),
            "Invalid handle"
        );

        let wrapper = unsafe { &*(self.buffer.as_ptr().add(byte_offset) as *const NodeWrapper<T>) };

        // Validate that the node tag matches the expected type
        assert_eq!(
            wrapper.info.node_type(),
            T::NODE_TYPE,
            "Node tag mismatch: expected {:?}, found {:?}",
            T::NODE_TYPE,
            wrapper.info.node_type()
        );
        wrapper
    }

    fn get_array_node_wrapper<T: AstNode + ArrayNode>(
        &self,
        handle: TypedNodeHandle<T>,
    ) -> &ArrayNodeWrapper<T> {
        let byte_offset = handle.byte_offset() as usize;
        assert!(
            byte_offset + size_of::<ArrayNodeWrapper<T>>() <= self.buffer.len(),
            "Invalid handle"
        );

        let wrapper =
            unsafe { &*(self.buffer.as_ptr().add(byte_offset) as *const ArrayNodeWrapper<T>) };

        // Validate that the node tag matches the expected type
        assert_eq!(
            wrapper.info.node_type(),
            T::NODE_TYPE,
            "Node tag mismatch: expected {:?}, found {:?}",
            T::NODE_TYPE,
            wrapper.info.node_type()
        );
        wrapper
    }

    // Get NodeInfo for any handle with validation
    pub fn get_info<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> NodeInfo {
        self.get_node_wrapper(handle).info
    }

    // Safe accessor for typed handles - validates node tag
    pub fn get<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> &T {
        &self.get_node_wrapper(handle).node
    }

    // Helper to get trailing array for typed handles - validates node tag
    pub fn get_array<T: AstNode + ArrayNode>(
        &self,
        handle: TypedNodeHandle<T>,
    ) -> &[T::ElementType] {
        let wrapper = self.get_array_node_wrapper(handle);
        unsafe {
            // Use the zero-sized array to get the perfect array start pointer
            let array_ptr = wrapper.array.as_ptr();
            std::slice::from_raw_parts(array_ptr, wrapper.array_len as usize)
        }
    }

    // Generic allocation for simple nodes
    pub fn alloc<T: AstNode>(&mut self, node: T, source_location: u32) -> TypedNodeHandle<T> {
        // Compile-time check that T doesn't implement Drop
        assert_no_drop::<T>();

        let wrapper = NodeWrapper {
            node,
            info: NodeInfo::new(T::NODE_TYPE, source_location),
        };

        let wrapper_size = size_of::<NodeWrapper<T>>();
        let align = align_of::<NodeWrapper<T>>().max(4); // Ensure at least 4-byte alignment

        // Pad to alignment
        let current_len = self.buffer.len();
        let aligned_offset = (current_len + align - 1) & !(align - 1);
        self.buffer.resize(aligned_offset, 0);
        let byte_offset = self.buffer.len();

        // Check 24-bit handle limit - now 4x larger (64MB - 4)
        // Must ensure the entire allocation fits within addressable range
        const MAX_SIZE: usize = ((1 << 24) - 1) * 4;
        assert!(
            byte_offset + wrapper_size <= MAX_SIZE,
            "Arena size exceeded handle limit (~64MB)"
        );

        self.buffer.reserve(wrapper_size);

        unsafe {
            let wrapper_ptr = self.buffer.as_mut_ptr().add(byte_offset) as *mut NodeWrapper<T>;
            std::ptr::write(wrapper_ptr, wrapper);
            self.buffer.set_len(byte_offset + wrapper_size);
        }

        TypedNodeHandle::new(byte_offset as u32)
    }

    // Generic allocation for nodes with trailing arrays
    pub fn alloc_with_array<T: AstNode + ArrayNode>(
        &mut self,
        node: T,
        array: &[T::ElementType],
        source_location: u32,
    ) -> TypedNodeHandle<T> {
        // Compile-time check that T doesn't implement Drop
        assert_no_drop::<T>();
        assert_no_drop::<T::ElementType>();

        assert!(
            array.len() <= u32::MAX as usize,
            "Array too large for u32 length"
        );

        let wrapper = ArrayNodeWrapper {
            node,
            info: NodeInfo::new(T::NODE_TYPE, source_location),
            array_len: array.len() as u32,
            array: [], // Zero-sized array
        };

        let wrapper_size = size_of::<ArrayNodeWrapper<T>>();
        let array_size = size_of::<T::ElementType>() * array.len();
        let total_size = wrapper_size + array_size;
        let align = align_of::<ArrayNodeWrapper<T>>().max(4); // Ensure at least 4-byte alignment

        // Pad to alignment
        let current_len = self.buffer.len();
        let aligned_offset = (current_len + align - 1) & !(align - 1);
        self.buffer.resize(aligned_offset, 0);
        let byte_offset = self.buffer.len();

        // Check 24-bit handle limit - now 4x larger (64MB - 4)
        // Must ensure the entire allocation fits within addressable range
        const MAX_SIZE: usize = ((1 << 24) - 1) * 4;
        assert!(
            byte_offset + total_size <= MAX_SIZE,
            "Arena size exceeded handle limit (~64MB)"
        );

        self.buffer.reserve(total_size);

        unsafe {
            let wrapper_ptr = self.buffer.as_mut_ptr().add(byte_offset) as *mut ArrayNodeWrapper<T>;

            // Write the wrapper
            std::ptr::write(wrapper_ptr, wrapper);

            // Write array data right after wrapper - using the zero-sized array to get perfect offset
            let array_ptr = (*wrapper_ptr).array.as_ptr() as *mut T::ElementType;
            std::ptr::copy_nonoverlapping(array.as_ptr(), array_ptr, array.len());

            self.buffer.set_len(byte_offset + total_size);
        }

        TypedNodeHandle::new(byte_offset as u32)
    }
}

#[ast_node(NodeType::TypeAtom)]
pub struct TypeAtomNode {
    pub atom: u32, // or whatever type you use for atoms
}

#[ast_node(NodeType::ConstUnit)]
pub struct ConstUnitNode {
    // Empty - all metadata is external
}

#[ast_node(NodeType::ConstBool)]
pub struct ConstBoolNode {
    pub value: bool,
}

#[ast_node(NodeType::ConstI32)]
pub struct ConstI32Node {
    pub value: i32,
}

#[ast_node(NodeType::ConstString, array = u8)]
pub struct ConstStringNode {
    // Empty - array length stored separately
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

#[ast_node(NodeType::Unop)]
pub struct UnopNode {
    pub op_type: UnopType,
    pub operand: NodeHandle,
}

#[ast_node(NodeType::Binop)]
pub struct BinopNode {
    pub op_type: BinopType,
    pub left: NodeHandle,
    pub right: NodeHandle,
}

#[ast_node(NodeType::DefineFn)]
pub struct DefineFnNode {
    pub local_ref: LocalRef,
    pub function_node: NodeHandle,
}

#[ast_node(NodeType::Define)]
pub struct DefineNode {
    pub local_ref: LocalRef,
    pub expr: Option<NodeHandle>,
}

#[ast_node(NodeType::Assign)]
pub struct AssignNode {
    pub local_ref: LocalRef,
    pub expr: NodeHandle,
}

#[ast_node(NodeType::LocalRead)]
pub struct LocalReadNode {
    pub local_ref: LocalRef,
}

#[ast_node(NodeType::Borrow)]
pub struct BorrowNode {
    pub mutable: bool,
    pub path_expr: NodeHandle,
}

#[ast_node(NodeType::Block, array = NodeHandle)]
pub struct BlockNode {
    pub block_index: BlockIndex,
}

#[ast_node(NodeType::While)]
pub struct WhileNode {
    pub is_inline: bool,
    pub cond: NodeHandle,
    pub body: TypedNodeHandle<BlockNode>,
}

#[ast_node(NodeType::If)]
pub struct IfNode {
    pub is_inline: bool,
    pub cond: NodeHandle,
    pub then_branch: NodeHandle,
    pub else_branch: NodeHandle,
}

#[ast_node(NodeType::Break)]
pub struct BreakNode {
    pub label: Option<Symbol>,
    pub value: NodeHandle,
}

#[ast_node(NodeType::Continue)]
pub struct ContinueNode {
    pub label: Option<Symbol>,
}

#[ast_node(NodeType::Return)]
pub struct ReturnNode {
    pub value_node: NodeHandle,
}

#[ast_node(NodeType::Fn, array = FnParam)]
pub struct FnNode {
    pub is_inline: bool,
    pub body: NodeHandle,
    pub return_type: NodeHandle,
}

#[ast_node(NodeType::Module)]
pub struct ModuleNode {
    pub body: NodeHandle,
}

#[ast_node(NodeType::Struct, array = StructField)]
pub struct StructNode {
    // Empty - all data is in the trailing array
}

// Type aliases and stub types (you'll need to define these based on your needs)
pub type LocalRef = u32;
pub type BlockIndex = u32;
pub type Symbol = u32;

// Non-node types that are stored in arrays
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct FnParam {
    pub name: Symbol,
    pub type_expr: NodeHandle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct StructField {
    pub name: Symbol,
    pub type_expr: NodeHandle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnopType {
    Neg,
    Not,
    // ... other unary operators
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinopType {
    Add,
    Sub,
    Mul,
    Div,
    // ... other binary operators
}

// Example usage
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_allocation() {
        let mut arena = AstArena::new();

        // Allocate some nodes using generic methods
        let const_handle = arena.alloc(ConstI32Node { value: 42 }, 0);
        let unit_handle = arena.alloc(ConstUnitNode {}, 0);

        // Access them safely with typed handles
        let const_node = arena.get(const_handle);
        assert_eq!(const_node.value, 42);

        let _unit_node = arena.get(unit_handle);
        assert_eq!(arena.get_info(unit_handle).source_location(), 0);

        // Can also convert to type-erased and use old API
        let erased_handle = const_handle.untyped();
        let typed_back = erased_handle
            .typed::<ConstI32Node>()
            .expect("Should convert back");
        let const_node2 = arena.get(typed_back);
        assert_eq!(const_node2.value, 42);
    }

    #[test]
    fn test_nodes_with_children() {
        let mut arena = AstArena::new();

        // Create some child nodes
        let child1 = arena.alloc(ConstI32Node { value: 1 }, 0);
        let child2 = arena.alloc(ConstI32Node { value: 2 }, 0);
        let children = [child1.untyped(), child2.untyped()]; // Automatic conversion to NodeHandle

        // Create a block with children
        let block_handle = arena.alloc_with_array(BlockNode { block_index: 0 }, &children, 0);

        // Access the block and its children
        let _block = arena.get(block_handle);
        let child_nodes = arena.get_array(block_handle);

        assert_eq!(child_nodes.len(), 2);
        assert_eq!(child_nodes[0], child1.untyped());
        assert_eq!(child_nodes[1], child2.untyped());
    }

    #[test]
    fn test_string_allocation() {
        let mut arena = AstArena::new();

        let test_string = "Hello, world!";
        let string_handle = arena.alloc_with_array(ConstStringNode {}, test_string.as_bytes(), 0);

        let _string_node = arena.get(string_handle);
        let retrieved_string = ConstStringNode::get_string(&arena, string_handle);

        assert_eq!(retrieved_string, test_string);
        assert_eq!(arena.get_array(string_handle).len(), test_string.len());
        assert_eq!(arena.get_info(string_handle).source_location(), 0);
    }

    #[test]
    fn test_fn_with_params() {
        let mut arena = AstArena::new();

        // Create some type nodes for parameters
        let int_type = arena.alloc(TypeAtomNode { atom: 1 }, 0); // assume 1 is int atom
        let bool_type = arena.alloc(TypeAtomNode { atom: 2 }, 0); // assume 2 is bool atom

        // Create function parameters
        let params = [
            FnParam {
                name: 100,
                type_expr: int_type.untyped(),
            }, // param1: int
            FnParam {
                name: 101,
                type_expr: bool_type.untyped(),
            }, // param2: bool
        ];

        // Create body and return type
        let body = arena.alloc(ConstUnitNode {}, 0);
        let return_type = arena.alloc(ConstUnitNode {}, 0);

        // Create function using generic allocation
        let fn_handle = arena.alloc_with_array(
            FnNode {
                is_inline: false,
                body: body.untyped(),
                return_type: return_type.untyped(),
            },
            &params,
            0,
        );

        // Access function and its parameters
        let _fn_node = arena.get(fn_handle);
        let fn_params = arena.get_array(fn_handle);

        assert_eq!(fn_params.len(), 2);
        assert_eq!(fn_params[0].name, 100);
        assert_eq!(fn_params[0].type_expr, int_type.untyped());
        assert_eq!(fn_params[1].name, 101);
        assert_eq!(fn_params[1].type_expr, bool_type.untyped());
    }

    #[test]
    fn test_struct_with_fields() {
        let mut arena = AstArena::new();

        // Create some type nodes
        let int_type = arena.alloc(TypeAtomNode { atom: 1 }, 0);
        let string_type = arena.alloc(TypeAtomNode { atom: 2 }, 0);

        // Create struct fields (not nodes!)
        let fields = [
            StructField {
                name: 200,
                type_expr: int_type.untyped(),
            },
            StructField {
                name: 201,
                type_expr: string_type.untyped(),
            },
        ];

        // Create struct
        let struct_handle = arena.alloc_with_array(StructNode {}, &fields, 0);

        // Access struct and its fields
        let _struct_node = arena.get(struct_handle);
        let struct_fields = arena.get_array(struct_handle);

        assert_eq!(struct_fields.len(), 2);
        assert_eq!(struct_fields[0].name, 200);
        assert_eq!(struct_fields[0].type_expr, int_type.untyped());
        assert_eq!(struct_fields[1].name, 201);
        assert_eq!(struct_fields[1].type_expr, string_type.untyped());
    }

    #[test]
    fn test_module_single_child() {
        let mut arena = AstArena::new();

        // Create a block as the module body
        let block_body = arena.alloc_with_array(BlockNode { block_index: 0 }, &[], 0);

        // Create module with single child
        let module_handle = arena.alloc(
            ModuleNode {
                body: block_body.untyped(),
            },
            0,
        );

        // Access module
        let module_node = arena.get(module_handle);
        assert_eq!(module_node.body, block_body.untyped());
    }

    #[test]
    fn test_typed_handle_conversion() {
        let mut arena = AstArena::new();

        let const_handle = arena.alloc(ConstI32Node { value: 42 }, 0);

        // Convert to erased handle
        let erased = const_handle.untyped(); // Using Into trait now

        // Convert back to typed handle
        let typed_again = erased.typed::<ConstI32Node>().expect("Should convert back");
        assert_eq!(typed_again, const_handle);

        // Try to convert to wrong type
        let wrong_typed = erased.typed::<ConstUnitNode>();
        assert!(wrong_typed.is_none());
    }

    #[test]
    fn test_lifetime_safety() {
        let mut arena = AstArena::new();
        let handle = arena.alloc(ConstI32Node { value: 42 }, 0);

        // This would prevent use-after-free at compile time:
        // let leaked_handle = {
        //     let mut temp_arena = AstArena::new();
        //     temp_arena.alloc(ConstI32Node { value: 1 }, 0) // handle tied to temp_arena lifetime
        // }; // temp_arena dropped here
        // arena.get(leaked_handle); // Compile error! handle has wrong lifetime

        // This works fine - handle tied to arena lifetime
        let node = arena.get(handle);
        assert_eq!(node.value, 42);
    }

    #[test]
    fn test_option_size_optimization() {
        use std::mem::size_of;

        // Test that Option<Handle> is the same size as Handle due to niche optimization
        println!("NodeHandle size: {}", size_of::<NodeHandle>());
        println!(
            "Option<NodeHandle> size: {}",
            size_of::<Option<NodeHandle>>()
        );
        println!("NonZeroU32 size: {}", size_of::<NonZeroU32>());
        println!(
            "Option<NonZeroU32> size: {}",
            size_of::<Option<NonZeroU32>>()
        );

        assert_eq!(size_of::<NodeHandle>(), size_of::<Option<NodeHandle>>());
        assert_eq!(
            size_of::<TypedNodeHandle<ConstI32Node>>(),
            size_of::<Option<TypedNodeHandle<ConstI32Node>>>()
        );

        // Verify they're the same size as the underlying NonZeroU32
        assert_eq!(size_of::<NodeHandle>(), size_of::<NonZeroU32>());
        assert_eq!(
            size_of::<TypedNodeHandle<ConstI32Node>>(),
            size_of::<NonZeroU32>()
        );

        let mut arena = AstArena::new();

        // Test that we can allocate at offset 0 now (no longer reserved)
        let handle = arena.alloc(ConstI32Node { value: 42 }, 0);
        assert_eq!(handle.byte_offset(), 0);

        // Test that None and Some have different representations
        let none_option: Option<TypedNodeHandle<ConstI32Node>> = None;
        let some_option = Some(handle);

        assert_ne!(none_option, some_option);

        // Test that we can use Option<Handle> in DefineNode
        let define_with_some = arena.alloc(
            DefineNode {
                local_ref: 1,
                expr: Some(handle.untyped()),
            },
            0,
        );
        let define_with_none = arena.alloc(
            DefineNode {
                local_ref: 2,
                expr: None,
            },
            0,
        );

        let node_some = arena.get(define_with_some);
        let node_none = arena.get(define_with_none);

        assert!(node_some.expr.is_some());
        assert!(node_none.expr.is_none());
    }
}
