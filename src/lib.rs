extern crate proc_macro;

use proc_macro2::{Span, TokenStream};

use quote::ToTokens;

use syn::{parse_macro_input, LitInt, LitStr, Result, Stmt};
use syn::parse::{Parse, ParseStream};

mod config;
mod handler;
mod engine;

use config::Config;
use handler::Handler;

trait Tactic: Parse {
    fn obfuscate(&self, config: Config) -> TokenStream;
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct Goldberg {
    statements: Vec<Stmt>,
}
impl Goldberg {
    fn new(statements: &Vec<Stmt>) -> Self {
        Self { statements: statements.clone() }
    }
}
impl Parse for Goldberg {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut statements = Vec::<Stmt>::new();

        while !input.is_empty() {
            let statement = input.parse::<Stmt>()?;
            statements.push(statement);
        }

        Ok(Self::new(&statements))
    }
}
impl Tactic for Goldberg {
    fn obfuscate(&self, config: Config) -> TokenStream {
        let handler = Handler::new(config);
        let stream = handler.statements(&self.statements);

        println!("obfuscated result:\n{}", stream.to_string());

        stream
    }
}

/// Obfuscate everything the Goldberg engine is capable of.
///
/// This will effectively attempt to obfuscate all items Goldberg is capable of within code. See more specific
/// macros (such as [`goldberg_code`](goldberg_code)) to only obfuscate one element.
#[proc_macro]
pub fn goldberg(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = parse_macro_input!(tokens as Goldberg);
    let config = Config::new(true, true, true);
    proc_macro::TokenStream::from(parsed.obfuscate(config))
}

/// Obfuscate like with the [`goldberg`](goldberg) macro, but return the code as a string.
///
/// Note that [`syn`](syn) and [`proc_macro2`](proc_macro2) don't pretty-format the code.
/// For that, you'll need to run the string through
/// [`rustfmt`](https://doc.rust-lang.org/book/appendix-04-useful-development-tools.html#automatic-formatting-with-rustfmt).
#[proc_macro]
pub fn goldberg_string(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = parse_macro_input!(tokens as Goldberg);
    let config = Config::new(true, true, true);
    let tokens = parsed.obfuscate(config);
    let string = tokens.to_string();
    let literal = LitStr::new(string.as_str(), Span::call_site());

    proc_macro::TokenStream::from(literal.to_token_stream())
}

/// Obfuscate only the control flow of the code.
#[proc_macro]
pub fn goldberg_code(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = parse_macro_input!(tokens as Goldberg);
    let config = Config::new(true, false, false);
    proc_macro::TokenStream::from(parsed.obfuscate(config))
}

#[proc_macro]
pub fn goldberg_int(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = parse_macro_input!(tokens as LitInt);

    println!("suffix: {}", parsed.suffix());

    parsed.to_token_stream().into()
}
