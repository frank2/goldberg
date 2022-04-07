use proc_macro2::{Ident, Span, TokenStream};

use quote::{quote, ToTokens};

use rand::prelude::*;

use syn::{Expr, parse2};

fn random_letter() -> char {
    let value = random::<u8>() % 26;

    if random::<bool>() { (value + 0x41) as char }
    else { (value + 0x61) as char }
}

fn random_identifier(length: usize) -> String {
    (0..length).map(|_| random_letter()).collect()
}

fn unique_ident(prefix: Option<&str>) -> Ident {
    if let Some(prefix_str) = prefix {
        Ident::new(format!("{}_{}", prefix_str, random_identifier(8)).as_str(), Span::call_site())
    }
    else {
        Ident::new(random_identifier(16).as_str(), Span::call_site())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Engine {}
impl Engine {
    pub fn new() -> Self { Self {} }

    pub fn flak_expr(&self, expr: TokenStream) -> TokenStream {
        println!("got expression: {}", expr.to_string());
        
        let value_1 = random::<u32>();
        let value_2 = random::<u32>();
        let val_ident = unique_ident(Some("x"));
        let vec_ident = unique_ident(Some("vec"));
        let reparsed = parse2::<Expr>(expr).unwrap();

        println!("parsed: {:?}", reparsed);

        let result = quote! {
            {
                let mut #val_ident = #value_1;
                let mut #vec_ident = Vec::<u32>::new();
                #val_ident ^= #value_2;
                #vec_ident .push(#val_ident);

                #reparsed
            }
        };

        println!("result: {}\n", result.to_string());

        result
    }

    pub fn loop_match(&self, statements: &Vec<TokenStream>) -> TokenStream {
        if statements.len() == 1 { return statements[0].clone(); }
        
        let mut idents = Vec::<u32>::new();

        for _ in 0..statements.len()+1 {
            let mut value = random::<u32>();

            while idents.contains(&value) {
                value = random::<u32>();
            }

            idents.push(value);
        }

        let mut keys = Vec::<u32>::new();

        for i in 0..idents.len()-1 {
            keys.push(idents[i] ^ idents[i+1]);
        }

        let mut keyed_streams = Vec::<(u32, u32, TokenStream)>::new();

        for i in 0..statements.len() {
            let ident = idents[i];
            let key = keys[i];
            keyed_streams.push((ident, key, statements[i].clone()));
        }

        let mut rng = thread_rng();
        keyed_streams.as_mut_slice().shuffle(&mut rng);

        let mut identified_streams = Vec::<TokenStream>::new();
        let ident_label = unique_ident(Some("ident"));
        let key_label = unique_ident(Some("key"));

        for i in 0..keyed_streams.len() {
            let (ident, key, stream) = keyed_streams[i].clone();
            
            identified_streams.push(quote! {
                #ident => {
                    #stream
                    #key_label = #key ;
                },
            });
        }

        let first_ident = idents[0];
        let exit_ident = idents[idents.len()-1];

        let loop_result = quote! {
            let mut #ident_label = #first_ident ;
            let mut #key_label = 0u32;

            loop {
                match #ident_label {
                    #(#identified_streams)*
                    #exit_ident => break,
                    _ => (),
                }

                #ident_label ^= #key_label ;
            }
        };

        loop_result
    }
}
