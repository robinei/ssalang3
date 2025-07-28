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
    LetFn,
    Let,
    Assign,
    Ident,
    Borrow,

    // Control flow nodesexpr
    Block,
    While,
    If,

    // Jump nodes
    Break,
    Continue,
    Return,

    // Function node
    Fn,

    // Struct nodes
    Struct,

    // Module node
    Module,
}
