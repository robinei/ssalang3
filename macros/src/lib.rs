use proc_macro::TokenStream;

mod ast_node_attr;

#[proc_macro_attribute]
pub fn ast_node(args: TokenStream, input: TokenStream) -> TokenStream {
    ast_node_attr::ast_node_attr(args, input)
}
