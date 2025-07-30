// Core node type enum - uses u8 for compact representation
// NodeType 0 is reserved for niche optimization (enables Option<Handle> same size as Handle)
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeType {
    Assign = 1, // Start at 1, reserve 0 for niche optimization
    Binop,
    Block,
    Borrow,
    Break,
    Call,
    ConstUnit,
    ConstBool,
    ConstInt,
    ConstUint,
    ConstFloat,
    ConstString,
    ConstTypeId,
    Continue,
    Fn,
    Ident,
    If,
    Let,
    Module,
    Return,
    Struct,
    Unop,
    While,
}
