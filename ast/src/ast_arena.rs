use crate::{ArrayLen, AstNode, CompileContext, CompileError, CompileResult, NodeType, nodes::*};
use std::{
    marker::PhantomData,
    mem::{align_of, size_of},
    num::NonZeroU32,
};

// NodeInfo struct that embeds source location and node tag
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct NodeInfo {
    // Lower 8 bits: node tag, upper 24 bits: source location
    data: u32,
}

impl NodeInfo {
    fn new(node_type: NodeType, source_location: u32) -> Self {
        debug_assert!(source_location < (1 << 24), "Source location too large for 24-bit storage");
        Self {
            data: (node_type as u32) | (source_location << 8),
        }
    }

    fn node_type(&self) -> NodeType {
        // Safety: We control construction and NodeType has valid discriminants 1-21
        unsafe { std::mem::transmute((self.data & 0xFF) as u8) }
    }

    fn source_location(&self) -> u32 {
        self.data >> 8
    }
}

#[repr(C)]
struct NodeWrapper<T: AstNode> {
    node: T,
    info: NodeInfo,
    array_len: T::LengthType,
    array: [T::ElementType; 0], // Zero-sized array to get offset of actual array data
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
            NodeType::Call => self.comp::<CallNode>(context),
            NodeType::ConstUnit => self.comp::<ConstUnitNode>(context),
            NodeType::ConstBool => self.comp::<ConstBoolNode>(context),
            NodeType::ConstInt => self.comp::<ConstIntNode>(context),
            NodeType::ConstUint => self.comp::<ConstUintNode>(context),
            NodeType::ConstFloat => self.comp::<ConstFloatNode>(context),
            NodeType::ConstString => self.comp::<ConstStringNode>(context),
            NodeType::ConstTypeId => self.comp::<ConstTypeIdNode>(context),
            NodeType::Continue => self.comp::<ContinueNode>(context),
            NodeType::Fn => self.comp::<FnNode>(context),
            NodeType::Ident => self.comp::<IdentNode>(context),
            NodeType::If => self.comp::<IfNode>(context),
            NodeType::Let => self.comp::<LetNode>(context),
            NodeType::Module => self.comp::<ModuleNode>(context),
            NodeType::Return => self.comp::<ReturnNode>(context),
            NodeType::Struct => self.comp::<StructNode>(context),
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
        debug_assert!(offset_div4 < (1 << 24), "Offset too large for 24-bit handle");
        let handle_bits = (T::NODE_TYPE as u32) | (offset_div4 << 8);
        debug_assert!(handle_bits != 0, "Handle should never be zero (node type 0 reserved for niche)");
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
        NodeHandle { handle: self.handle }
    }

    #[inline]
    pub fn compile(self, context: &mut CompileContext) -> CompileResult {
        let result = context.module.ast.get(self).compile(context, self)?;
        if context.static_eval && !result.is_const() {
            return Err(CompileError::Error(Some(self.untyped()), "expected const value".to_string()));
        }
        Ok(result)
    }
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

    // Private wrapper accessor methods
    #[inline]
    fn get_node_wrapper<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> &NodeWrapper<T> {
        let byte_offset = handle.byte_offset() as usize;
        assert!(byte_offset + size_of::<NodeWrapper<T>>() <= self.buffer.len(), "Invalid handle");

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

    // Get source location for any handle with validation
    #[inline]
    pub fn get_source_location<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> u32 {
        self.get_node_wrapper(handle).info.source_location()
    }

    // Safe accessor for typed handles - validates node tag
    #[inline]
    pub fn get<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> &T {
        &self.get_node_wrapper(handle).node
    }

    // Helper to get trailing array for typed handles - validates node tag
    #[inline]
    pub fn get_array<T: AstNode>(&self, handle: TypedNodeHandle<T>) -> &[T::ElementType] {
        let wrapper = self.get_node_wrapper(handle);
        unsafe {
            // Use the zero-sized array to get the perfect array start pointer
            let array_ptr = wrapper.array.as_ptr();
            let array_len: u32 = wrapper.array_len.get_len();
            std::slice::from_raw_parts(array_ptr, array_len as usize)
        }
    }

    // Generic allocation for simple nodes
    #[inline]
    pub fn alloc<T: AstNode>(&mut self, node: T, source_location: u32) -> TypedNodeHandle<T> {
        self.alloc_with_array(node, &[], source_location)
    }

    // Generic allocation for nodes with trailing arrays
    pub fn alloc_with_array<T: AstNode>(&mut self, node: T, array: &[T::ElementType], source_location: u32) -> TypedNodeHandle<T> {
        // Compile-time check that T doesn't implement Drop
        assert_no_drop::<T>();
        assert_no_drop::<T::ElementType>();

        assert!(array.len() <= u32::MAX as usize, "Array too large for u32 length");

        let wrapper = NodeWrapper {
            node,
            info: NodeInfo::new(T::NODE_TYPE, source_location),
            array_len: T::LengthType::create_len(array.len() as u32),
            array: [], // Zero-sized array
        };

        let total_size = size_of::<NodeWrapper<T>>() + size_of::<T::ElementType>() * array.len();
        let align = align_of::<NodeWrapper<T>>().max(4); // Ensure at least 4-byte alignment

        // Pad to alignment
        let current_len = self.buffer.len();
        let aligned_offset = (current_len + align - 1) & !(align - 1);
        self.buffer.resize(aligned_offset, 0);
        let byte_offset = self.buffer.len();

        // Check 24-bit handle limit - now 4x larger (64MB - 4)
        // Must ensure the entire allocation fits within addressable range
        const MAX_SIZE: usize = ((1 << 24) - 1) * 4;
        assert!(byte_offset + total_size <= MAX_SIZE, "Arena size exceeded handle limit (~64MB)");

        self.buffer.reserve(total_size);

        unsafe {
            let wrapper_ptr = self.buffer.as_mut_ptr().add(byte_offset) as *mut NodeWrapper<T>;
            std::ptr::write(wrapper_ptr, wrapper);

            // Write array data right after wrapper - using the zero-sized array to get perfect offset
            if size_of::<T::ElementType>() > 0 {
                let array_ptr = (*wrapper_ptr).array.as_ptr() as *mut T::ElementType;
                std::ptr::copy_nonoverlapping(array.as_ptr(), array_ptr, array.len());
            }

            self.buffer.set_len(byte_offset + total_size);
        }

        TypedNodeHandle::new(byte_offset as u32)
    }

    // Create handle from node reference - safe but efficient
    pub fn handle_from_ref<T: AstNode>(&self, node_ref: &T) -> Option<TypedNodeHandle<T>> {
        let node_ptr = node_ref as *const T;
        let buffer_start = self.buffer.as_ptr();
        let buffer_end = unsafe { buffer_start.add(self.buffer.len()) };

        // Check if the reference points within our buffer
        if (node_ptr as *const u8) < buffer_start || (node_ptr as *const u8) >= buffer_end {
            return None;
        }

        // Calculate potential wrapper pointer by backing up from node to wrapper start
        let wrapper_ptr = unsafe { (node_ptr as *const u8).sub(std::mem::offset_of!(NodeWrapper<T>, node)) } as *const NodeWrapper<T>;

        // Verify wrapper pointer is within bounds and properly aligned
        if (wrapper_ptr as *const u8) < buffer_start
            || (wrapper_ptr as *const u8) >= buffer_end
            || (wrapper_ptr as usize) % std::mem::align_of::<NodeWrapper<T>>() != 0
        {
            return None;
        }

        let byte_offset = unsafe { (wrapper_ptr as *const u8).offset_from(buffer_start) } as u32;

        // Verify 4-byte alignment for handle
        if byte_offset % 4 != 0 {
            return None;
        }

        // Validate the node type matches
        let wrapper = unsafe { &*wrapper_ptr };
        if wrapper.info.node_type() != T::NODE_TYPE {
            return None;
        }

        // Additional bounds check - ensure entire wrapper + array fits
        let array_len = wrapper.array_len.get_len() as usize;
        let total_size = size_of::<NodeWrapper<T>>() + size_of::<T::ElementType>() * array_len;
        if byte_offset as usize + total_size > self.buffer.len() {
            return None;
        }

        Some(TypedNodeHandle::new(byte_offset))
    }
}
