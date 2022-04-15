//! [Goldberg](https://github.com/frank2/goldberg) is a Rust obfuscation macro library.
//!
//! Currently, Goldberg is capable of the following types of obfuscation:
//! * code-flow obfuscation
//! * string literal encryption
//! * integer literal obfuscation
//!
//! Of course, some caveats apply:
//! * code-flow obfuscation only applies to statements that have mobility (see [`goldberg_stmts`](goldberg_stmts!)).
//! * string literal encryption produces temporary objects (see [`goldberg_string`](goldberg_string!)).
//! * integer literals must be typed (see [`goldberg_int`](goldberg_int!)).
//!
//! Despite these caveats, these simple techniques produce powerfully annoying obfuscation that survives code
//! optimization.
//!
//! ```rust
//! use goldberg::goldberg_stmts;
//!
//! let result = goldberg_stmts! {
//!    {
//!       fn print(value: u32) {
//!          let msg = String::from("value:");
//!          println!("{} {}", msg, value);
//!       }
//!
//!       let mut x: u32 = 0xDEADBEEFu32;
//!       print(x);
//!       x ^= 0xFACEBABEu32;
//!       print(x);
//!       x ^= 0xDEFACED1u32;
//!       print(x);
//!       x ^= 0xABAD1DEAu32;
//!       print(x);
//!       x
//!    }
//! };
//!
//! assert_eq!(result, 0x5134d76a);
//! ```
//!
//! This example expands into code similar to this:
//! ```no_run
//! fn print(value: u32) {
//!     let msg = String::from(
//!         {
//!             let key_fgnliibu: Vec<u8> = vec![75u8, 87u8, 169u8, 234u8, 230u8, 38u8];
//!             let mut string_hkzmkgaw: Vec<u8> = vec![61u8, 54u8, 197u8, 159u8, 131u8, 28u8];
//!             for pulhfjddcbiztuxz in 0..string_hkzmkgaw.len() {
//!                 string_hkzmkgaw[pulhfjddcbiztuxz] ^= key_fgnliibu[pulhfjddcbiztuxz];
//!             }
//!             String::from_utf8(string_hkzmkgaw).unwrap()
//!         }
//!         .as_str(),
//!     );
//!     println!("{} {}", msg, value);
//! }
//! struct _AssertDefault_cfygodkf
//! where
//!     u32: Default;
//! let mut x: u32 = u32::default();
//! let mut ident_gqtkhobp = 1113386507u32;
//! let mut key_ftudpieg = 0u32;
//! 'loop_obfu_jmcfjvhq: loop {
//!     match ident_gqtkhobp {
//!         2158235392u32 => {
//!             print(x);
//!             key_ftudpieg = 3044081204u32;
//!         }
//!         2506875858u32 => {
//!             x ^= {
//!                 struct _AssertDefault_vedfwrhy
//!                 where
//!                     u32: Default;
//!                 let mut calc_whsuusro: u32 = u32::default();
//!                 let mut ident_tmheadmi = 1821101871u32;
//!                 let mut key_pzediytf = 0u32;
//!                 'loop_obfu_msqcffqh: loop {
//!                     match ident_tmheadmi {
//!                         1103538895u32 => {
//!                             calc_whsuusro ^= 2534362044u32;
//!                             key_pzediytf = 2755681459u32;
//!                         }
//!                         3757011920u32 => {
//!                             calc_whsuusro = calc_whsuusro.swap_bytes();
//!                             key_pzediytf = 849856391u32;
//!                         }
//!                         1071321848u32 => {
//!                             calc_whsuusro = calc_whsuusro.rotate_left(1692640787u32);
//!                             key_pzediytf = 1375898541u32;
//!                         }
//!                         ...
//! ```
//!
//! For obfuscating statements, use [`goldberg_stmts`](goldberg_stmts!). For encrypting strings, use [`goldberg_string`](goldberg_string!).
//! For integers, use [`goldberg_int`](goldberg_int!). To convert obfuscated statements into a string for external processing,
//! use [`goldberg_stringify`](goldberg_stringify!). For functional examples, read
//! [the test file](https://github.com/frank2/goldberg/blob/main/tests/tests.rs).
extern crate proc_macro;

use moisture::Moisture;

use proc_macro2::{Span, TokenStream};

use quote::ToTokens;

use syn::LitStr;

mod engine;

/// Obfuscate a series of statements.
///
/// This implicitly applies [`goldberg_string`](goldberg_string!) and [`goldberg_int`](goldberg_int!)
/// obfuscation to corresponding string literals and integer literals as well as code-flow obfuscation.
///
/// Statements are obfuscated by randomizing their apparent order with loop/match obfuscation. This
/// determines order with a rolling encryption key which identifies which statement to execute. To accomplish
/// this, a statement in the statements must be *mobile*. A statement is mobile if:
///
/// * It is not the last expression in the series of statements
/// * It is not an [item](https://doc.rust-lang.org/reference/items.html).
/// * It is a typed `let` statement (e.g., `let x: u32 = 0`) and the type implements the [Default](std::default::Default) trait.
#[proc_macro]
pub fn goldberg_stmts(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream = TokenStream::from(tokens);
    let mut moisture = Moisture::new();

    engine::register_callbacks(&mut moisture);
    proc_macro::TokenStream::from(engine::stmts_entry(&moisture, stream))
}

/// Obfuscate with the [`goldberg_stmts`](goldberg_stmts!) macro, but return the code as a string.
///
/// Note that [syn](syn) and [proc-macro2](proc_macro2) don't pretty-format the code.
/// For that, you'll need to run the string through
/// [`rustfmt`](https://doc.rust-lang.org/book/appendix-04-useful-development-tools.html#automatic-formatting-with-rustfmt).
#[proc_macro]
pub fn goldberg_stringify(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream = TokenStream::from(tokens);
    let mut moisture = Moisture::new();

    engine::register_callbacks(&mut moisture);

    let tokens = engine::stmts_entry(&moisture, stream);
    let string = tokens.to_string();
    let literal = LitStr::new(string.as_str(), Span::call_site());

    proc_macro::TokenStream::from(literal.to_token_stream())
}

/// Obfuscate an integer literal.
///
/// This requires that the integer literal has a type suffix (i.e., the `u32` in `0xDEADBEEFu32`). Obfuscation
/// will not take effect on an integer without a type.
#[proc_macro]
pub fn goldberg_int(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream = TokenStream::from(tokens);
    let mut moisture = Moisture::new();

    engine::register_callbacks(&mut moisture);
    proc_macro::TokenStream::from(engine::int_entry(&moisture, stream))
}

/// Obfuscate (or rather, encrypt) a string literal.
///
/// Due to the nature of string literals, in order for this to work, the macro must return a *temporary object* back
/// to the call site. Therefore, the obfuscated string literal must be used right away (e.g., an argument to a
/// function call). For long-lasting obfuscated string literals, wrap the macro in [`String::from`](String::from).
#[proc_macro]
pub fn goldberg_string(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream = TokenStream::from(tokens);
    let mut moisture = Moisture::new();

    engine::register_callbacks(&mut moisture);
    proc_macro::TokenStream::from(engine::str_entry(&moisture, stream))
}
