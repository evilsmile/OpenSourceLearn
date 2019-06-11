// proc_macro crate comes with Rust.
// 'proc_macro' crate is the compiler's API that allow us to read 
// and manipulate Rust code from our code.
extern crate proc_macro;

use crate::proc_macro::TokenStream;
// 'syn' crate parses Rust code from a string into a data structure 
// that we can perform operations on.
use syn;
// 'quote' crate turns 'syn' data structures back into Rust code.
use quote::quote;
// These crates make it much simpler to parse any sort of Rust code
// we might want to handle.

#[proc_macro_derive(HelloMacro)]
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_hello_macro(&ast)
}

fn impl_hello_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote!{
        impl HelloMacro for #name {
            fn hello_macro() {
                println!("Hello, Macro! My name is {}", stringify!(#name));
            }
        }
    };

    gen.into()
}
