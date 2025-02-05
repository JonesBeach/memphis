extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, ItemFn};

/// Macro to generate test cases for all known interpreters
#[proc_macro_attribute]
pub fn test(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let func_name = &input.sig.ident;

    // Generate valid test function names
    let bytecode_vm_test_name = format_ident!("{}_bytecode_vm", func_name);
    let treewalk_test_name = format_ident!("{}_treewalk", func_name);

    let generated_tests = quote! {
        #[test]
        fn #bytecode_vm_test_name() {
            #func_name(Adapter(Box::new(BytecodeVmAdapter::new())));
        }

        #[test]
        fn #treewalk_test_name() {
            #func_name(Adapter(Box::new(TreewalkAdapter::new())));
        }
    };

    let output = quote! {
        #input  // This is needed to keep the original test function name in the final Rust code
        #generated_tests
    };

    TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn test_with(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let func_name = &input.sig.ident;

    // Parse arguments like `BytecodeVmAdapter, TreewalkAdapter`
    let args = attr.to_string().replace(" ", ""); // Remove spaces
    let interpreters: Vec<&str> = args.split(',').collect();

    let mut generated_tests = quote! {};

    for interpreter in interpreters {
        let test_name = format!("{}_{}", func_name, interpreter.to_lowercase());
        let test_ident = syn::Ident::new(&test_name, proc_macro2::Span::call_site());

        let instantiation = match interpreter {
            "BytecodeVmAdapter" => quote! { Adapter(Box::new(BytecodeVmAdapter::new())) },
            "TreewalkAdapter" => quote! { Adapter(Box::new(TreewalkAdapter::new())) },
            _ => panic!("Unknown interpreter: {}", interpreter),
        };

        generated_tests.extend(quote! {
            #[test]
            fn #test_ident() {
                #func_name(#instantiation);
            }
        });
    }

    let output = quote! {
        #input  // This is needed to keep the original test function name in the final Rust code
        #generated_tests
    };

    TokenStream::from(output)
}
