use moisture::{CallbackType, Context, Moisture, run_moisture};

use proc_macro2::{Ident, Span, TokenStream};

use quote::{quote, quote_spanned, ToTokens};

use rand::prelude::*;

use syn::*;
use syn::parse::Parser;
use syn::spanned::Spanned;

fn random_letter() -> char {
    let value = random::<u8>() % 26;

    (value + 0x61) as char
}

fn random_identifier(length: usize) -> String {
    (0..length).map(|_| random_letter()).collect()
}

fn random_key(length: usize) -> Vec<u8> {
    (0..length).map(|_| random::<u8>()).collect()
}

fn unique_ident(prefix: Option<&str>) -> Ident {
    if let Some(prefix_str) = prefix {
        Ident::new(format!("{}_{}", prefix_str, random_identifier(8)).as_str(), Span::call_site())
    }
    else {
        Ident::new(random_identifier(16).as_str(), Span::call_site())
    }
}

pub fn register_callbacks(moisture: &mut Moisture) {
    moisture.register_callback(CallbackType::Stmts, stmts_handler);
    moisture.register_callback(CallbackType::LitStr, lit_str_handler);
    moisture.register_callback(CallbackType::LitInt, lit_int_handler);
}

pub fn stmts_entry(moisture: &Moisture, tokens: TokenStream) -> TokenStream {
    run_moisture!(moisture, CallbackType::Stmts, tokens)
}

pub fn str_entry(moisture: &Moisture, tokens: TokenStream) -> TokenStream {
    run_moisture!(moisture, CallbackType::LitStr, tokens)
}

pub fn int_entry(moisture: &Moisture, tokens: TokenStream) -> TokenStream {
    run_moisture!(moisture, CallbackType::LitInt, tokens)
}

pub fn loop_match(statements: &Vec<TokenStream>) -> TokenStream {
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
    let loop_label_str = format!("'loop_obfu_{}", random_identifier(8));
    let loop_label = Lifetime::new(loop_label_str.as_str(), Span::call_site());

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

        #loop_label : loop {
            match #ident_label {
                #(#identified_streams)*
                #exit_ident => break #loop_label ,
                _ => (),
            }

            #ident_label ^= #key_label ;
        }
    };

    loop_result
}
    
pub fn expr_is_mobile(expr: &Expr) -> bool {
    match expr {
        Expr::Assign(_) => true,
        Expr::AssignOp(_) => true,
        Expr::Async(_) => true,
        Expr::Await(_) => true,
        Expr::Block(_) => true,
        Expr::Box(_) => true,
        Expr::Break(_) => true,
        Expr::Call(_) => true,
        Expr::Continue(_) => true,
        Expr::Field(_) => true,
        Expr::ForLoop(_) => true,
        Expr::If(_) => true,
        Expr::Loop(_) => true,
        Expr::Macro(_) => true,
        Expr::Match(_) => true,
        Expr::MethodCall(_) => true,
        Expr::Paren(_) => true,
        Expr::Return(_) => true,
        Expr::Struct(_) => true,
        Expr::TryBlock(_) => true,
        Expr::Unsafe(_) => true,
        Expr::While(_) => true,
        Expr::Yield(_) => true,
        _ => false,
    }
}

pub fn local_path_to_init_path(path: &TypePath) -> TokenStream {
    let TypePath {
        qself,
        path } = path;
    let mut result = TokenStream::new();
    let mut tokens = Vec::<TokenStream>::new();

    if let Some(_) = qself {
        panic!("can't handle qself properly");
    }

    if let Some(lead) = path.leading_colon {
        tokens.push(lead.to_token_stream());
    }

    let mut segments = Vec::<TokenStream>::new();

    for segment in &path.segments {
        let PathSegment {
            ident,
            arguments } = segment;

        segments.push(ident.to_token_stream());

        if let PathArguments::None = arguments { continue; }
        
        if let PathArguments::AngleBracketed(generic_args) = arguments {
            // segments.push(quote! { :: });
            segments.push(generic_args.to_token_stream());
        }
    }

    tokens.push(quote! { #(#segments)::* });

    result.extend(tokens.into_iter());
    result
}

pub fn split_local(moisture: &Moisture, context: &Context, local: &Local) -> Result<(TokenStream, TokenStream, Option<TokenStream>)> {
    let Local {
        attrs: _,
        let_token: _,
        pat,
        init,
        semi_token: _ } = local.clone();

    let pat_type;

    if let Pat::Type(type_data) = pat {
        pat_type = type_data;
    }
    else { return Err(Error::new(local.span(), "Local declaration splitting without PatType pattern")); }

    let PatType {
        attrs: _,
        pat: type_ident,
        colon_token: _,
        ty } = pat_type;

    let fixed_ident;
    
    if let Pat::Ident(ref pat_ident) = *type_ident {
        let PatIdent {
            attrs,
            by_ref: _,
            mutability: _,
            ident,
            subpat: _ } = pat_ident;

        fixed_ident = quote! { #(#attrs)* #ident };
    }
    else {
        fixed_ident = type_ident.to_token_stream();
    }
    
    let assert_tokens;
    let declare_tokens;

    if let Type::Path(ref type_path) = *ty {
        let default_path = local_path_to_init_path(type_path);
        let ty_span = ty.span();
        let assert_token = unique_ident(Some("_AssertDefault"));

        assert_tokens = quote_spanned! { ty_span=> struct #assert_token where #ty: Default; };
        declare_tokens = quote! {
            let mut #fixed_ident: #ty = #default_path::default();
        };
    }
    else { return Err(Error::new(ty.span(), "Local type does not resolve into a Path segment")); }

    if let Some((_, init_expr)) = init {
        let new_expr = moisture.callback(context, CallbackType::Expr, init_expr.to_token_stream())?;
        let init_tokens = quote! { #fixed_ident = #new_expr; };

        Ok((assert_tokens, declare_tokens, Some(init_tokens)))
    }
    else { Ok((assert_tokens, declare_tokens, None)) }
}

pub fn stmts_handler(moisture: &Moisture, context: &Context, tokens: TokenStream) -> Result<TokenStream> {
    let statements = Block::parse_within.parse2(tokens)?;
    let mut stream = TokenStream::new();
    let mut statement_streams = Vec::<TokenStream>::new();
    let mut loop_match_candidates = Vec::<TokenStream>::new();
    let mut loop_match_declarations = Vec::<TokenStream>::new();
        
    for i in 0..statements.len()-1 {
        let stmt = &statements[i];
            
        match stmt {
            Stmt::Local(ref local) => {
                if let Pat::Type(_) = local.pat {
                    let (assert, declare, init) = split_local(moisture, context, local)?;
                    loop_match_declarations.push(assert);
                    loop_match_declarations.push(declare);

                    if let Some(init_stmt) = init {
                        loop_match_candidates.push(init_stmt);
                    }

                    continue;
                }
                else if loop_match_candidates.len() > 0 {
                    loop_match_declarations.iter().for_each(|x| statement_streams.push(x.clone()));
                    statement_streams.push(loop_match(&loop_match_candidates));
                    
                    loop_match_declarations.clear();
                    loop_match_candidates.clear();
                }

                let result = moisture.callback(context, CallbackType::Local, local.to_token_stream())?;
                statement_streams.push(result);
            },
            Stmt::Item(ref item) => {
                if loop_match_candidates.len() > 0 {
                    loop_match_declarations.iter().for_each(|x| statement_streams.push(x.clone()));
                    statement_streams.push(loop_match(&loop_match_candidates));
                    
                    loop_match_declarations.clear();
                    loop_match_candidates.clear();
                }

                let result = moisture.callback(context, CallbackType::Item, item.to_token_stream())?;
                statement_streams.push(result);
            },
            Stmt::Expr(ref expr) => {
                if expr_is_mobile(expr) {
                    let new_expr = moisture.callback(context, CallbackType::Expr, expr.to_token_stream())?;
                    loop_match_candidates.push(new_expr);
                    continue;
                }
                else if loop_match_candidates.len() > 0 {
                    loop_match_declarations.iter().for_each(|x| statement_streams.push(x.clone()));
                    statement_streams.push(loop_match(&loop_match_candidates));
                    
                    loop_match_declarations.clear();
                    loop_match_candidates.clear();
                }
                
                let new_expr = moisture.callback(context, CallbackType::Expr, expr.to_token_stream())?;
                statement_streams.push(new_expr);
            },
            Stmt::Semi(ref expr, _) => {
                if expr_is_mobile(expr) {
                    let new_expr = moisture.callback(context, CallbackType::Expr, expr.to_token_stream())?;
                    loop_match_candidates.push(quote! { #new_expr ; });
                    continue;
                }
                else if loop_match_candidates.len() > 0 {
                    loop_match_declarations.iter().for_each(|x| statement_streams.push(x.clone()));
                    statement_streams.push(loop_match(&loop_match_candidates));
                    
                    loop_match_declarations.clear();
                    loop_match_candidates.clear();
                }
                
                let new_expr = moisture.callback(context, CallbackType::Expr, expr.to_token_stream())?;
                statement_streams.push(quote! { #new_expr ; });
            },
        }
    }

    if loop_match_candidates.len() > 0 {
        loop_match_declarations.iter().for_each(|x| statement_streams.push(x.clone()));
        statement_streams.push(loop_match(&loop_match_candidates));
    }

    let final_statement = &statements[statements.len()-1];
    let result = match final_statement {
        Stmt::Local(ref local) => moisture.callback(context, CallbackType::Local, local.to_token_stream()),
        Stmt::Item(ref item) => moisture.callback(context, CallbackType::Item, item.to_token_stream()),
        Stmt::Expr(ref expr) => moisture.callback(context, CallbackType::Expr, expr.to_token_stream()),
        Stmt::Semi(ref expr, _) => moisture.callback(context, CallbackType::Expr, expr.to_token_stream()),
    }?;

    if let Stmt::Semi(_, _) = final_statement {
        statement_streams.push(quote! { #result ; });
    }
    else {
        statement_streams.push(result);
    }
        
    stream.extend(statement_streams.into_iter());
        
    Ok(stream)
}

pub fn lit_str_handler(_: &Moisture, context: &Context, tokens: TokenStream) -> Result<TokenStream> {
    let lit_str = parse2::<LitStr>(tokens)?;

    // Pattern objects can't be turned into expressions, so the obfuscation technique will fail to compile
    if let Some((CallbackType::PatLit, _)) = context.peek(4) {
        return Ok(lit_str.to_token_stream());
    }
    
    Ok(lit_str_obfu(&lit_str))
}

pub fn lit_str_obfu(str_obj: &LitStr) -> TokenStream {
    let str_data = str_obj.value();

    if str_data.len() == 0 { return str_obj.to_token_stream(); }
    
    let str_bytes: Vec<u8> = str_data.as_str().as_bytes().iter().copied().collect();
    let key = random_key(str_bytes.len());
    let mut encrypted = str_bytes.clone();

    for i in 0..key.len() {
        encrypted[i] ^= key[i];
    }

    let key_ident = unique_ident(Some("key"));
    let str_ident = unique_ident(Some("string"));
    let index_ident = unique_ident(None);

    let result = quote! {
        {
            let #key_ident: Vec<u8> = vec![#(#key),*];
            let mut #str_ident: Vec<u8> = vec![#(#encrypted),*];

            for #index_ident in 0..#str_ident.len() {
                #str_ident[#index_ident] ^= #key_ident[#index_ident];
            }

            String::from_utf8(#str_ident).unwrap()
        }.as_str()
    };
    
    result
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum IntType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    Isize,
    Usize,
}
impl IntType {
    pub fn from_suffix(suffix: &str) -> Self {
        match suffix {
            "i8" => Self::I8,
            "u8" => Self::U8,
            "i16" => Self::I16,
            "u16" => Self::U16,
            "i32" => Self::I32,
            "u32" => Self::U32,
            "i64" => Self::I64,
            "u64" => Self::U64,
            "i128" => Self::I128,
            "u128" => Self::U128,
            "isize" => Self::Isize,
            "usize" => Self::Usize,
            _ => panic!("bad suffix"),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum IntOperand {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    I128(i128),
    U128(u128),
    Isize(isize),
    Usize(usize),
}
impl IntOperand {
    pub fn random(ty: IntType) -> Self {
        match ty {
            IntType::I8 => Self::I8(random::<i8>()),
            IntType::U8 => Self::U8(random::<u8>()),
            IntType::I16 => Self::I16(random::<i16>()),
            IntType::U16 => Self::U16(random::<u16>()),
            IntType::I32 => Self::I32(random::<i32>()),
            IntType::U32 => Self::U32(random::<u32>()),
            IntType::I64 => Self::I64(random::<i64>()),
            IntType::U64 => Self::U64(random::<u64>()),
            IntType::I128 => Self::I128(random::<i128>()),
            IntType::U128 => Self::U128(random::<u128>()),
            IntType::Isize => Self::Isize(random::<isize>()),
            IntType::Usize => Self::Usize(random::<usize>()),
        }
    }
    pub fn add(&self, op: Self) -> Self {
        match self {
            Self::I8(lh) => {
                if let Self::I8(rh) = op { Self::I8(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U8(lh) => {
                if let Self::U8(rh) = op { Self::U8(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I16(lh) => {
                if let Self::I16(rh) = op { Self::I16(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U16(lh) => {
                if let Self::U16(rh) = op { Self::U16(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I32(lh) => {
                if let Self::I32(rh) = op { Self::I32(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U32(lh) => {
                if let Self::U32(rh) = op { Self::U32(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I64(lh) => {
                if let Self::I64(rh) = op { Self::I64(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U64(lh) => {
                if let Self::U64(rh) = op { Self::U64(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I128(lh) => {
                if let Self::I128(rh) = op { Self::I128(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U128(lh) => {
                if let Self::U128(rh) = op { Self::U128(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::Isize(lh) => {
                if let Self::Isize(rh) = op { Self::Isize(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::Usize(lh) => {
                if let Self::Usize(rh) = op { Self::Usize(lh.wrapping_add(rh)) }
                else { panic!("operand type mismatch"); }
            },
        }
    }
    pub fn sub(&self, op: Self) -> Self {
        match self {
            Self::I8(lh) => {
                if let Self::I8(rh) = op { Self::I8(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U8(lh) => {
                if let Self::U8(rh) = op { Self::U8(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I16(lh) => {
                if let Self::I16(rh) = op { Self::I16(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U16(lh) => {
                if let Self::U16(rh) = op { Self::U16(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I32(lh) => {
                if let Self::I32(rh) = op { Self::I32(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U32(lh) => {
                if let Self::U32(rh) = op { Self::U32(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I64(lh) => {
                if let Self::I64(rh) = op { Self::I64(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U64(lh) => {
                if let Self::U64(rh) = op { Self::U64(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::I128(lh) => {
                if let Self::I128(rh) = op { Self::I128(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::U128(lh) => {
                if let Self::U128(rh) = op { Self::U128(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::Isize(lh) => {
                if let Self::Isize(rh) = op { Self::Isize(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
            Self::Usize(lh) => {
                if let Self::Usize(rh) = op { Self::Usize(lh.wrapping_sub(rh)) }
                else { panic!("operand type mismatch"); }
            },
        }
    }    
    pub fn xor(&self, op: Self) -> Self {
        match self {
            Self::I8(lh) => {
                if let Self::I8(rh) = op { Self::I8(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::U8(lh) => {
                if let Self::U8(rh) = op { Self::U8(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::I16(lh) => {
                if let Self::I16(rh) = op { Self::I16(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::U16(lh) => {
                if let Self::U16(rh) = op { Self::U16(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::I32(lh) => {
                if let Self::I32(rh) = op { Self::I32(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::U32(lh) => {
                if let Self::U32(rh) = op { Self::U32(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::I64(lh) => {
                if let Self::I64(rh) = op { Self::I64(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::U64(lh) => {
                if let Self::U64(rh) = op { Self::U64(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::I128(lh) => {
                if let Self::I128(rh) = op { Self::I128(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::U128(lh) => {
                if let Self::U128(rh) = op { Self::U128(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::Isize(lh) => {
                if let Self::Isize(rh) = op { Self::Isize(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
            Self::Usize(lh) => {
                if let Self::Usize(rh) = op { Self::Usize(lh ^ rh) }
                else { panic!("operand type mismatch"); }
            },
        }
    }
    pub fn rotate_left(&self, amt: u32) -> Self {
        match self {
            Self::I8(v) => Self::I8(v.rotate_left(amt)),
            Self::U8(v) => Self::U8(v.rotate_left(amt)),
            Self::I16(v) => Self::I16(v.rotate_left(amt)),
            Self::U16(v) => Self::U16(v.rotate_left(amt)),
            Self::I32(v) => Self::I32(v.rotate_left(amt)),
            Self::U32(v) => Self::U32(v.rotate_left(amt)),
            Self::I64(v) => Self::I64(v.rotate_left(amt)),
            Self::U64(v) => Self::U64(v.rotate_left(amt)),
            Self::I128(v) => Self::I128(v.rotate_left(amt)),
            Self::U128(v) => Self::U128(v.rotate_left(amt)),
            Self::Isize(v) => Self::Isize(v.rotate_left(amt)),
            Self::Usize(v) => Self::Usize(v.rotate_left(amt)),
        }
    }
    pub fn rotate_right(&self, amt: u32) -> Self {
        match self {
            Self::I8(v) => Self::I8(v.rotate_right(amt)),
            Self::U8(v) => Self::U8(v.rotate_right(amt)),
            Self::I16(v) => Self::I16(v.rotate_right(amt)),
            Self::U16(v) => Self::U16(v.rotate_right(amt)),
            Self::I32(v) => Self::I32(v.rotate_right(amt)),
            Self::U32(v) => Self::U32(v.rotate_right(amt)),
            Self::I64(v) => Self::I64(v.rotate_right(amt)),
            Self::U64(v) => Self::U64(v.rotate_right(amt)),
            Self::I128(v) => Self::I128(v.rotate_right(amt)),
            Self::U128(v) => Self::U128(v.rotate_right(amt)),
            Self::Isize(v) => Self::Isize(v.rotate_right(amt)),
            Self::Usize(v) => Self::Usize(v.rotate_right(amt)),
        }
    }
    pub fn swap_bytes(&self) -> Self {
        match self {
            Self::I8(v) => Self::I8(v.swap_bytes()),
            Self::U8(v) => Self::U8(v.swap_bytes()),
            Self::I16(v) => Self::I16(v.swap_bytes()),
            Self::U16(v) => Self::U16(v.swap_bytes()),
            Self::I32(v) => Self::I32(v.swap_bytes()),
            Self::U32(v) => Self::U32(v.swap_bytes()),
            Self::I64(v) => Self::I64(v.swap_bytes()),
            Self::U64(v) => Self::U64(v.swap_bytes()),
            Self::I128(v) => Self::I128(v.swap_bytes()),
            Self::U128(v) => Self::U128(v.swap_bytes()),
            Self::Isize(v) => Self::Isize(v.swap_bytes()),
            Self::Usize(v) => Self::Usize(v.swap_bytes()),
        }
    }
    pub fn reverse_bits(&self) -> Self {
        match self {
            Self::I8(v) => Self::I8(v.reverse_bits()),
            Self::U8(v) => Self::U8(v.reverse_bits()),
            Self::I16(v) => Self::I16(v.reverse_bits()),
            Self::U16(v) => Self::U16(v.reverse_bits()),
            Self::I32(v) => Self::I32(v.reverse_bits()),
            Self::U32(v) => Self::U32(v.reverse_bits()),
            Self::I64(v) => Self::I64(v.reverse_bits()),
            Self::U64(v) => Self::U64(v.reverse_bits()),
            Self::I128(v) => Self::I128(v.reverse_bits()),
            Self::U128(v) => Self::U128(v.reverse_bits()),
            Self::Isize(v) => Self::Isize(v.reverse_bits()),
            Self::Usize(v) => Self::Usize(v.reverse_bits()),
        }
    }
}
impl From<LitInt> for IntOperand {
    fn from(lit: LitInt) -> Self {
        let ty = IntType::from_suffix(lit.suffix());

        match ty {
            IntType::I8 => Self::I8(lit.base10_parse::<i8>().unwrap()),
            IntType::U8 => Self::U8(lit.base10_parse::<u8>().unwrap()),
            IntType::I16 => Self::I16(lit.base10_parse::<i16>().unwrap()),
            IntType::U16 => Self::U16(lit.base10_parse::<u16>().unwrap()),
            IntType::I32 => Self::I32(lit.base10_parse::<i32>().unwrap()),
            IntType::U32 => Self::U32(lit.base10_parse::<u32>().unwrap()),
            IntType::I64 => Self::I64(lit.base10_parse::<i64>().unwrap()),
            IntType::U64 => Self::U64(lit.base10_parse::<u64>().unwrap()),
            IntType::I128 => Self::I128(lit.base10_parse::<i128>().unwrap()),
            IntType::U128 => Self::U128(lit.base10_parse::<u128>().unwrap()),
            IntType::Isize => Self::Isize(lit.base10_parse::<isize>().unwrap()),
            IntType::Usize => Self::Usize(lit.base10_parse::<usize>().unwrap()),
        }
    }
}
impl ToTokens for IntOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::I8(v) => v.to_tokens(tokens),
            Self::U8(v) => v.to_tokens(tokens),
            Self::I16(v) => v.to_tokens(tokens),
            Self::U16(v) => v.to_tokens(tokens),
            Self::I32(v) => v.to_tokens(tokens),
            Self::U32(v) => v.to_tokens(tokens),
            Self::I64(v) => v.to_tokens(tokens),
            Self::U64(v) => v.to_tokens(tokens),
            Self::I128(v) => v.to_tokens(tokens),
            Self::U128(v) => v.to_tokens(tokens),
            Self::Isize(v) => v.to_tokens(tokens),
            Self::Usize(v) => v.to_tokens(tokens),
        }
    }
}
            
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum IntOperation {
    Add(IntOperand),
    Sub(IntOperand),
    Xor(IntOperand),
    RotateLeft(u32),
    RotateRight(u32),
    SwapBytes,
    ReverseBits,
}
impl IntOperation {
    pub fn random(ty: IntType) -> Self {
        let operation = random::<u8>() % 7;

        match operation {
            0 => Self::Add(IntOperand::random(ty)),
            1 => Self::Sub(IntOperand::random(ty)),
            2 => Self::Xor(IntOperand::random(ty)),
            3 => Self::RotateLeft(random::<u32>()),
            4 => Self::RotateRight(random::<u32>()),
            5 => Self::SwapBytes,
            _ => Self::ReverseBits,
        }
    }
    pub fn invert(&self) -> Self {
        match self {
            Self::Add(op) => Self::Sub(*op),
            Self::Sub(op) => Self::Add(*op),
            Self::Xor(op) => Self::Xor(*op),
            Self::RotateLeft(count) => Self::RotateRight(*count),
            Self::RotateRight(count) => Self::RotateLeft(*count),
            Self::SwapBytes => Self::SwapBytes,
            Self::ReverseBits => Self::ReverseBits,
        }
    }
    pub fn perform(&self, lh: IntOperand) -> IntOperand {
        match self {
            Self::Add(rh) => lh.add(*rh),
            Self::Sub(rh) => lh.sub(*rh),
            Self::Xor(rh) => lh.xor(*rh),
            Self::RotateLeft(amt) => lh.rotate_left(*amt),
            Self::RotateRight(amt) => lh.rotate_right(*amt),
            Self::SwapBytes => lh.swap_bytes(),
            Self::ReverseBits => lh.reverse_bits(),
        }
    }
    pub fn to_token_stream(&self, ident: Ident) -> TokenStream {
        match self {
            Self::Add(op) => quote! { #ident = #ident.wrapping_add(#op) ; },
            Self::Sub(op) => quote! { #ident = #ident.wrapping_sub(#op) ; },
            Self::Xor(op) => quote! { #ident ^= #op ; },
            Self::RotateLeft(amt) => quote! { #ident = #ident.rotate_left(#amt) ; },
            Self::RotateRight(amt) => quote! { #ident = #ident.rotate_right(#amt) ; },
            Self::SwapBytes => quote! { #ident = #ident.swap_bytes(); },
            Self::ReverseBits => quote! { #ident = #ident.reverse_bits(); },
        }
    }
}

pub fn lit_int_handler(moisture: &Moisture, context: &Context, tokens: TokenStream) -> Result<TokenStream> {
    let lit_int = parse2::<LitInt>(tokens)?;

    // Pattern objects can't be turned into expressions, so the obfuscation technique will fail to compile
    if let Some((CallbackType::PatLit, _)) = context.peek(4) {
        return Ok(lit_int.to_token_stream());
    }
    // Don't handle untyped integers
    else if lit_int.suffix().len() == 0 {
        return Ok(lit_int.to_token_stream());
    }
    // We're already inside a LitInt parser, just return the token stream to
    // prevent infinite recursion
    else if context.contains(CallbackType::LitInt) {
        return Ok(lit_int.to_token_stream());
    }

    let result = lit_int_obfu(moisture, context, &lit_int)?;
    
    Ok(result)
}

pub fn lit_int_obfu(moisture: &Moisture, context: &Context, lit: &LitInt) -> Result<TokenStream> {
    let int_type = IntType::from_suffix(lit.suffix());
    let op_count = random::<usize>() % 10 + 5;
    let mut operations = Vec::<IntOperation>::new();
    let mut calc_op = IntOperand::from(lit.clone());

    while operations.len() < op_count {
        let new_op = IntOperation::random(int_type);
        
        calc_op = new_op.perform(calc_op);
        operations.push(new_op);
    }

    let new_ident = unique_ident(Some("calc"));
    let type_ident = Ident::new(lit.suffix(), Span::call_site());
    let inverted_ops: Vec<IntOperation> = operations.iter().rev().map(|x| x.invert()).collect();
    let mut stmts = Vec::<TokenStream>::new();
    
    stmts.push(quote! { let mut #new_ident : #type_ident  = #calc_op ; });

    for op in &inverted_ops {
        stmts.push(op.to_token_stream(new_ident.clone()));
    }

    stmts.push(quote! { #new_ident });

    let new_block = quote! {
        {
            #(#stmts)*
        }
    };

    let obfu_block = moisture.callback(context, CallbackType::Block, new_block)?;

    Ok(obfu_block)
}
