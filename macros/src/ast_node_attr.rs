use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Token, parse_macro_input};

// Helper for parsing `NodeType::Block, array = NodeHandle`
struct AstNodeArgs {
    node_type: syn::Path,
    array_type: Option<syn::Type>,
}

impl syn::parse::Parse for AstNodeArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let node_type: syn::Path = input.parse()?;

        let array_type = if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            input.parse::<syn::Ident>()?; // "array"
            input.parse::<Token![=]>()?;
            Some(input.parse::<syn::Type>()?)
        } else {
            None
        };

        Ok(AstNodeArgs {
            node_type,
            array_type,
        })
    }
}

pub fn ast_node_attr(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let args = parse_macro_input!(args as AstNodeArgs);

    let struct_name = &input.ident;
    let node_type = &args.node_type;

    let (array_impl, element_type) = if let Some(ref elem_type) = args.array_type {
        (
            quote! { impl ArrayNode for #struct_name {} },
            quote! { type ElementType = #elem_type; },
        )
    } else {
        (quote! {}, quote! { type ElementType = (); })
    };

    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(C)]
        #input

        impl AstNode for #struct_name {
            const NODE_TYPE: NodeType = #node_type;
            #element_type
        }

        #array_impl
    }
    .into()
}
