use proc_macro2::TokenStream;

use quote::{quote, ToTokens};

use syn::*;
use syn::token::Semi;

use crate::config::Config;
use crate::engine::Engine;

#[derive(Clone, Debug)]
pub struct Handler {
    config: Config,
    engine: Engine,
}
impl Handler {
    pub fn new(config: Config) -> Self { Self { config, engine: Engine::new() } }
    
    pub fn local(&self, local: &Local) -> TokenStream {
        let attrs = local.attrs.clone();
        let let_token = local.let_token.clone();
        let pat = local.pat.clone();
        let init = local.init.clone();
        let semi = local.semi_token.clone();

        let mut stream = TokenStream::new();
        let mut stream_vec = Vec::<TokenStream>::new();

        stream_vec.push(quote! {
            #(#attrs)*
            #let_token #pat
        });

        if let Some((equal, expr)) = init {
            let new_expr = self.expr(&expr);
            stream_vec.push(quote! { #equal #new_expr });
        }

        stream_vec.push(semi.to_token_stream());
        stream.extend(stream_vec.into_iter());

        stream
    }

    pub fn item(&self, item: &Item) -> TokenStream {
        match item {
            Item::Const(ref const_) => self.const_(const_),
            Item::Fn(ref func) => self.func(func),
            Item::Impl(ref impl_) => self.impl_(impl_),
            Item::Mod(ref mod_) => self.mod_(mod_),
            Item::Static(ref static_) => self.static_(static_),
            Item::Trait(ref trait_) => self.trait_(trait_),
            _ => item.to_token_stream(),
        }
    }

    pub fn const_(&self, const_: &ItemConst) -> TokenStream {
        let attrs = const_.attrs.clone();
        let vis = const_.vis.clone();
        let token = const_.const_token.clone();
        let ident = const_.ident.clone();
        let colon_token = const_.colon_token.clone();
        let ty = const_.ty.clone();
        let eq_token = const_.eq_token.clone();
        let expr = self.expr(&const_.expr);
        let semi = const_.semi_token.clone();

        quote! {
            #(#attrs)*
            #vis #token #ident #colon_token #ty #eq_token #expr #semi
        }
    }

    pub fn func(&self, func: &ItemFn) -> TokenStream {
        let attrs = func.attrs.clone();
        let vis = func.vis.clone();
        let sig = func.sig.clone();
        let block = self.block(&func.block);

        quote! {
            #(#attrs)*
            #vis #sig
            #block
        }
    }

    pub fn impl_(&self, impl_: &ItemImpl) -> TokenStream {
        let attrs = impl_.attrs.clone();
        let defaultness = impl_.defaultness.clone();
        let unsafety = impl_.unsafety.clone();
        let token = impl_.impl_token.clone();
        let generics = impl_.generics.clone();
        let trait_ = impl_.trait_.clone();
        let self_ty = impl_.self_ty.clone();
        let items = impl_.items.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
        });

        if let Some(default_keyword) = defaultness {
            tokens.push(quote! { #default_keyword });
        }

        if let Some(unsafe_keyword) = unsafety {
            tokens.push(quote! { #unsafe_keyword });
        }

        tokens.push(quote! {
            #token #generics
        });

        if let Some((option_bang, path, for_)) = trait_ {
            if let Some(bang_token) = option_bang {
                tokens.push(quote! { #bang_token });
            }

            tokens.push(quote! { #path #for_ });
        }

        tokens.push(quote! { #self_ty });
        
        let mut filtered_items = Vec::<TokenStream>::new();

        for impl_item in &items {
            filtered_items.push(match impl_item {
                ImplItem::Const(ref impl_const) => self.impl_item_const(impl_const),
                ImplItem::Method(ref method) => self.impl_item_method(method),
                _ => impl_item.to_token_stream(),
            });
        }
        
        tokens.push(quote! {
            {
                #(#filtered_items)*
            }
        });

        result.extend(tokens.into_iter());
        result
    }

    pub fn impl_item_const(&self, impl_const: &ImplItemConst) -> TokenStream {
        let ImplItemConst {
            attrs,
            vis,
            defaultness,
            const_token,
            ident,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token } = impl_const.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #vis
        });

        if let Some(default_token) = defaultness {
            tokens.push(quote! { #default_token });
        }

        tokens.push(quote! {
            #const_token #ident #colon_token #ty #eq_token
        });

        tokens.push(self.expr(&expr));
        tokens.push(quote! { #semi_token });

        result.extend(tokens.into_iter());
        result
    }

    pub fn impl_item_method(&self, method: &ImplItemMethod) -> TokenStream {
        let ImplItemMethod {
            attrs,
            vis,
            defaultness,
            sig,
            block } = method.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #vis
        });

        if let Some(default_token) = defaultness {
            tokens.push(quote! { #default_token });
        }

        tokens.push(quote! { #sig });
        tokens.push(self.block(&block));

        result.extend(tokens.into_iter());
        result
    }

    pub fn mod_(&self, mod_: &ItemMod) -> TokenStream {
        let ItemMod {
            attrs,
            vis,
            mod_token,
            ident,
            content,
            semi } = mod_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #vis #mod_token #ident
        });

        if let Some((_, items)) = content {
            let mut filtered_items = Vec::<TokenStream>::new();

            for item in &items {
                filtered_items.push(self.item(item));
            }

            tokens.push(quote! {
                {
                    #(#filtered_items)*
                }
            });
        }

        tokens.push(quote! { #semi });

        result.extend(tokens.into_iter());
        result
    }

    pub fn static_(&self, static_: &ItemStatic) -> TokenStream {
        let ItemStatic {
            attrs,
            vis,
            static_token,
            mutability,
            ident,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token } = static_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #vis #static_token
        });

        if let Some(mutable) = mutability {
            tokens.push(quote! { #mutable });
        }

        tokens.push(quote! {
            #ident #colon_token #ty #eq_token
        });
        
        tokens.push(self.expr(&expr));
        
        tokens.push(quote! { #semi_token });

        result.extend(tokens.into_iter());
        result
    }

    pub fn trait_(&self, trait_: &ItemTrait) -> TokenStream {
        let ItemTrait {
            attrs,
            vis,
            unsafety,
            auto_token,
            trait_token,
            ident,
            generics,
            colon_token,
            supertraits,
            brace_token: _,
            items } = trait_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #vis
        });

        if let Some(unsafe_token) = unsafety {
            tokens.push(quote! { #unsafe_token });
        }

        if let Some(auto_keyword) = auto_token {
            tokens.push(quote! { #auto_keyword });
        }

        tokens.push(quote! { #trait_token #ident #generics });

        if let Some(colon) = colon_token {
            tokens.push(quote! { #colon });
        }

        tokens.push(quote! { #supertraits });

        let mut filtered_items = Vec::<TokenStream>::new();

        for item in &items {
            filtered_items.push(match item {
                TraitItem::Const(ref const_) => self.trait_item_const(const_),
                TraitItem::Method(ref method) => self.trait_item_method(method),
                _ => item.to_token_stream(),
            });
        }

        tokens.push(quote! {
            {
                #(#filtered_items)*
            }
        });

        result.extend(tokens.into_iter());
        result
    }

    pub fn trait_item_const(&self, const_: &TraitItemConst) -> TokenStream {
        let TraitItemConst {
            attrs,
            const_token,
            ident,
            colon_token,
            ty,
            default,
            semi_token } = const_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #const_token #ident #colon_token #ty
        });

        if let Some((eq_token, expr)) = default {
            let new_expr = self.expr(&expr);

            tokens.push(quote! { #eq_token #new_expr });
        }

        tokens.push(quote! { #semi_token });

        result.extend(tokens.into_iter());
        result
    }

    pub fn trait_item_method(&self, method: &TraitItemMethod) -> TokenStream {
        let TraitItemMethod {
            attrs,
            sig,
            default,
            semi_token } = method.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)* #sig
        });

        if let Some(block) = default {
            tokens.push(self.block(&block));
        }

        if let Some(semi) = semi_token {
            tokens.push(quote! { #semi });
        }

        result.extend(tokens.into_iter());
        result
    }
    
    pub fn block(&self, block: &Block) -> TokenStream {
        println!("found block: {}", block.to_token_stream().to_string());
        
        let new_statements = self.statements(&block.stmts);

        quote! {
            {
                #new_statements
            }
        }
    }

    pub fn expr(&self, expr: &Expr) -> TokenStream {
        /*
        if let Expr::Binary(_) = expr {
            return self.engine.flak_expr(expr.to_token_stream());
        }
        if let Expr::Lit(_) = expr {
            return self.engine.flak_expr(expr.to_token_stream());
        }
         */
        
        self.engine.flak_expr(match expr {
            Expr::Assign(ref assign) => self.expr_assign(assign),
            Expr::AssignOp(ref assign_op) => self.expr_assign_op(assign_op),
            Expr::Async(ref async_) => self.expr_async(async_),
            Expr::Await(ref await_) => self.expr_await(await_),
            Expr::Block(ref block) => self.expr_block(block),
            Expr::Box(ref box_) => self.expr_box(box_),
            Expr::Break(ref break_) => self.expr_break(break_),
            Expr::Call(ref call) => self.expr_call(call),
            Expr::Cast(ref cast) => self.expr_cast(cast),
            Expr::Closure(ref closure) => self.expr_closure(closure),
            Expr::Field(ref field) => self.expr_field(field),
            Expr::ForLoop(ref for_loop) => self.expr_for_loop(for_loop),
            Expr::Group(ref group) => self.expr_group(group),
            Expr::If(ref if_) => self.expr_if(if_),
            Expr::Index(ref index) => self.expr_index(index),
            Expr::Let(ref let_) => self.expr_let(let_),
            Expr::Loop(ref loop_) => self.expr_loop(loop_),
            Expr::Match(ref match_) => self.expr_match(match_),
            Expr::MethodCall(ref method) => self.expr_method_call(method),
            Expr::Paren(ref paren) => self.expr_paren(paren),
            Expr::Range(ref range) => self.expr_range(range),
            Expr::Reference(ref reference) => self.expr_reference(reference),
            Expr::Repeat(ref repeat) => self.expr_repeat(repeat),
            Expr::Return(ref return_) => self.expr_return(return_),
            Expr::Struct(ref struct_) => self.expr_struct(struct_),
            Expr::Try(ref try_) => self.expr_try(try_),
            Expr::TryBlock(ref try_block) => self.expr_try_block(try_block),
            Expr::Tuple(ref tuple) => self.expr_tuple(tuple),
            Expr::Type(ref type_) => self.expr_type(type_),
            Expr::Unary(ref unary) => self.expr_unary(unary),
            Expr::Unsafe(ref unsafe_) => self.expr_unsafe(unsafe_),
            Expr::While(ref while_) => self.expr_while(while_),
            Expr::Yield(ref yield_) => self.expr_yield(yield_),
            _ => expr.to_token_stream(),
        })
    }
    
    pub fn expr_is_mobile(&self, expr: &Expr) -> bool {
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

    pub fn expr_assign(&self, assign: &ExprAssign) -> TokenStream {
        let ExprAssign {
            attrs,
            left,
            eq_token,
            right } = assign.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        // the left-hand expression of an assignment is an lvalue, which is very difficult to
        // obfuscate. just leave it alone.
        tokens.push(quote! { #left });
        tokens.push(quote! { #eq_token });
        tokens.push(self.expr(&right));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_assign_op(&self, assign_op: &ExprAssignOp) -> TokenStream {
        let ExprAssignOp {
            attrs,
            left,
            op,
            right } = assign_op.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        // the left-hand expression of an assignment is an lvalue, which is very difficult to
        // obfuscate. just leave it alone.
        tokens.push(quote! { #left });
        tokens.push(quote! { #op });
        tokens.push(self.expr(&right));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_async(&self, async_: &ExprAsync) -> TokenStream {
        let ExprAsync {
            attrs,
            async_token,
            capture,
            block } = async_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)* #async_token
        });

        if let Some(move_token) = capture {
            tokens.push(quote! { #move_token });
        }

        tokens.push(self.block(&block));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_await(&self, await_: &ExprAwait) -> TokenStream {
        let ExprAwait {
            attrs,
            base,
            dot_token,
            await_token } = await_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&base));
        tokens.push(quote! { #dot_token });
        tokens.push(quote! { #await_token });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_block(&self, block: &ExprBlock) -> TokenStream {
        let attrs = block.attrs.clone();
        let new_block = self.block(&block.block);
    
        quote! {
            #(#attrs)*
            #new_block
        }
    }

    pub fn expr_box(&self, box_: &ExprBox) -> TokenStream {
        let ExprBox {
            attrs,
            box_token,
            expr } = box_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(quote! { #box_token });
        tokens.push(self.expr(&expr));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_break(&self, break_: &ExprBreak) -> TokenStream {
        let ExprBreak {
            attrs,
            break_token,
            label,
            expr } = break_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #break_token
        });

        if let Some(lifetime) = label {
            tokens.push(quote! { #lifetime });
        }

        if let Some(break_expr) = expr {
            tokens.push(self.expr(&break_expr));
        }

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_call(&self, call: &ExprCall) -> TokenStream {
        let ExprCall {
            attrs,
            func,
            paren_token: _,
            args } = call.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&func));

        let mut filtered_args = Vec::<TokenStream>::new();

        for arg_expr in &args {
            filtered_args.push(self.expr(arg_expr));
        }

        tokens.push(quote! {
            ( #(#filtered_args),* )
        });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_cast(&self, cast: &ExprCast) -> TokenStream {
        let ExprCast {
            attrs,
            expr,
            as_token,
            ty } = cast.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&expr));
        tokens.push(quote! { #as_token #ty });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_closure(&self, closure: &ExprClosure) -> TokenStream {
        let ExprClosure {
            attrs,
            movability,
            asyncness,
            capture,
            or1_token,
            inputs,
            or2_token,
            output,
            body } = closure.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });

        if let Some(static_token) = movability {
            tokens.push(quote! { #static_token });
        }

        if let Some(async_token) = asyncness {
            tokens.push(quote! { #async_token });
        }

        if let Some(move_token) = capture {
            tokens.push(quote! { #move_token });
        }

        tokens.push(quote! {
            #or1_token #inputs #or2_token #output
        });
        tokens.push(self.expr(&body));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_field(&self, field: &ExprField) -> TokenStream {
        let ExprField {
            attrs,
            base,
            dot_token,
            member } = field.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&base));
        tokens.push(quote! { #dot_token });
        tokens.push(quote! { #member });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_for_loop(&self, for_loop: &ExprForLoop) -> TokenStream {
        let ExprForLoop {
            attrs,
            label,
            for_token,
            pat,
            in_token,
            expr,
            body } = for_loop.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #label #for_token #pat #in_token
        });
        tokens.push(self.expr(&expr));
        tokens.push(self.block(&body));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_group(&self, group: &ExprGroup) -> TokenStream {
        let ExprGroup {
            attrs,
            group_token: _, // this might cause a bug, but I don't know how to test for it
            expr } = group.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&expr));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_if(&self, if_: &ExprIf) -> TokenStream {
        let ExprIf {
            attrs,
            if_token,
            cond,
            then_branch,
            else_branch } = if_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #if_token
        });
        tokens.push(self.expr(&cond));
        tokens.push(self.block(&then_branch));

        if let Some((else_token, else_expr)) = else_branch {
            let filtered_else = self.expr(&else_expr);

            tokens.push(quote! { #else_token #filtered_else });
        }

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_index(&self, index: &ExprIndex) -> TokenStream {
        let ExprIndex {
            attrs,
            expr,
            bracket_token: _,
            index } = index.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&expr));

        let filtered_index = self.expr(&index);

        tokens.push(quote! { [ #filtered_index ] });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_let(&self, let_: &ExprLet) -> TokenStream {
        let ExprLet {
            attrs,
            let_token,
            pat,
            eq_token,
            expr } = let_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #let_token #pat #eq_token
        });
        tokens.push(self.expr(&expr));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_loop(&self, loop_: &ExprLoop) -> TokenStream {
        let ExprLoop {
            attrs,
            label,
            loop_token,
            body } = loop_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });

        if let Some(label_token) = label {
            tokens.push(quote! { #label_token });
        }

        tokens.push(quote! { #loop_token });
        tokens.push(self.block(&body));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_match(&self, match_: &ExprMatch) -> TokenStream {
        let ExprMatch {
            attrs,
            match_token,
            expr,
            brace_token: _,
            arms } = match_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)* #match_token
        });
        tokens.push(self.expr(&expr));

        let mut filtered_arms = Vec::<TokenStream>::new();

        for arm in &arms {
            filtered_arms.push(self.arm(arm));
        }

        tokens.push(quote! {
            {
                #(#filtered_arms)*
            }
        });

        result.extend(tokens.into_iter());
        result
    }

    pub fn arm(&self, arm: &Arm) -> TokenStream {
        let Arm {
            attrs,
            pat,
            guard,
            fat_arrow_token,
            body,
            comma } = arm.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! {
            #(#attrs)*
            #pat
        });

        if let Some((guard_if, guard_expr)) = guard {
            let filtered_expr = self.expr(&guard_expr);

            tokens.push(quote! { #guard_if #filtered_expr });
        }

        tokens.push(quote! { #fat_arrow_token });
        tokens.push(self.expr(&body));

        if let Some(comma_token) = comma {
            tokens.push(quote! { #comma_token });
        }

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_method_call(&self, method: &ExprMethodCall) -> TokenStream {
        let ExprMethodCall {
            attrs,
            receiver,
            dot_token,
            method,
            turbofish,
            paren_token: _,
            args } = method.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&receiver));
        tokens.push(quote! { #dot_token #method });

        if let Some(turbo) = turbofish {
            tokens.push(quote! { #turbo });
        }

        let mut filtered_args = Vec::<TokenStream>::new();

        for arg in &args {
            filtered_args.push(self.expr(arg));
        }

        tokens.push(quote! { ( #(#filtered_args),* ) });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_paren(&self, paren: &ExprParen) -> TokenStream {
        let ExprParen {
            attrs,
            paren_token: _,
            expr } = paren.clone();

        let filtered_expr = self.expr(&expr);

        quote! {
            #(#attrs)*
            ( #filtered_expr )
        }
    }

    pub fn expr_range(&self, range: &ExprRange) -> TokenStream {
        let ExprRange {
            attrs,
            from,
            limits,
            to } = range.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });

        if let Some(from_expr) = from {
            tokens.push(self.expr(&from_expr));
        }

        // this is a small bug with the implementation in syn,
        // RangeLimits should implement ToTokens but doesn't. should be fixed
        // when my PR gets published.
        // tokens.push(quote! { #limits });
        tokens.push(match limits {
            RangeLimits::HalfOpen(dot2) => dot2.to_token_stream(),
            RangeLimits::Closed(dot_eq) => dot_eq.to_token_stream(),
        });

        if let Some(to_expr) = to {
            tokens.push(self.expr(&to_expr));
        }

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_reference(&self, reference: &ExprReference) -> TokenStream {
        let ExprReference {
            attrs,
            and_token,
            raw: _,
            mutability,
            expr } = reference.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* #and_token });

        if let Some(mut_token) = mutability {
            tokens.push(quote! { #mut_token });
        }

        tokens.push(self.expr(&expr));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_repeat(&self, repeat: &ExprRepeat) -> TokenStream {
        let ExprRepeat {
            attrs,
            bracket_token: _,
            expr,
            semi_token,
            len } = repeat.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&expr));
        tokens.push(quote! { #semi_token });
        tokens.push(self.expr(&len));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_return(&self, return_: &ExprReturn) -> TokenStream {
        let ExprReturn {
            attrs,
            return_token,
            expr } = return_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* #return_token });

        if let Some(opt_expr) = expr {
            let filtered_expr = self.expr(&opt_expr);
            tokens.push(quote! { #filtered_expr });
        }

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_struct(&self, struct_: &ExprStruct) -> TokenStream {
        let ExprStruct {
            attrs,
            path,
            brace_token: _,
            fields,
            dot2_token,
            rest } = struct_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* #path });

        let mut filtered_fields = Vec::<TokenStream>::new();
        let mut substream = Vec::<TokenStream>::new();
             
        for field in &fields {
            filtered_fields.push(self.field(field));
        }

        substream.push(quote! { #(#filtered_fields),* });

        if let Some(dot2) = dot2_token {
            substream.push(quote! { #dot2 });
        }

        if let Some(rest_expr) = rest {
            substream.push(self.expr(&rest_expr));
        }

        tokens.push(quote! { { #(#substream)* } });

        result.extend(tokens.into_iter());
        result
    }

    pub fn field(&self, field: &FieldValue) -> TokenStream {
        let FieldValue {
            attrs,
            member,
            colon_token,
            expr } = field.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* #member });

        if let Some(colon) = colon_token {
            tokens.push(quote! { #colon });
        }

        tokens.push(self.expr(&expr));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_try(&self, try_: &ExprTry) -> TokenStream {
        let ExprTry {
            attrs,
            expr,
            question_token } = try_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&expr));
        tokens.push(quote! { #question_token });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_try_block(&self, try_block: &ExprTryBlock) -> TokenStream {
        let ExprTryBlock {
            attrs,
            try_token,
            block } = try_block.clone();

        let filtered_block = self.block(&block);

        quote! { #(#attrs)* #try_token #filtered_block }
    }

    pub fn expr_tuple(&self, tuple: &ExprTuple) -> TokenStream {
        let ExprTuple {
            attrs,
            paren_token: _,
            elems } = tuple.clone();

        let mut filtered_elems = Vec::<TokenStream>::new();

        for elem in &elems {
            filtered_elems.push(self.expr(elem));
        }

        quote! { #(#attrs)* ( #(#filtered_elems),* ) }
    }

    pub fn expr_type(&self, type_: &ExprType) -> TokenStream {
        let ExprType {
            attrs,
            expr,
            colon_token: _,
            ty } = type_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });
        tokens.push(self.expr(&expr));
        tokens.push(quote! { #ty });

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_unary(&self, unary: &ExprUnary) -> TokenStream {
        let ExprUnary {
            attrs,
            op,
            expr } = unary.clone();

        let filtered_expr = self.expr(&expr);

        quote! { #(#attrs)* #op #filtered_expr }
    }

    pub fn expr_unsafe(&self, unsafe_: &ExprUnsafe) -> TokenStream {
        let ExprUnsafe {
            attrs,
            unsafe_token,
            block } = unsafe_.clone();

        let filtered_block = self.block(&block);

        quote! { #(#attrs)* #unsafe_token #filtered_block }
    }

    pub fn expr_while(&self, while_: &ExprWhile) -> TokenStream {
        let ExprWhile {
            attrs,
            label,
            while_token,
            cond,
            body } = while_.clone();

        let mut result = TokenStream::new();
        let mut tokens = Vec::<TokenStream>::new();

        tokens.push(quote! { #(#attrs)* });

        if let Some(label_token) = label {
            tokens.push(quote! { #label_token });
        }

        tokens.push(quote! { #while_token });
        tokens.push(self.expr(&cond));
        tokens.push(self.block(&body));

        result.extend(tokens.into_iter());
        result
    }

    pub fn expr_yield(&self, yield_: &ExprYield) -> TokenStream {
        let ExprYield {
            attrs,
            yield_token,
            expr } = yield_.clone();

        if let Some(opt_expr) = expr {
            let filtered_expr = self.expr(&opt_expr);

            quote! { #(#attrs)* #yield_token #filtered_expr }
        }
        else { quote! { #(#attrs)* #yield_token } }
    }

    pub fn semi(&self, expr: &Expr, semi: &Semi) -> TokenStream {
        let expr_tokens = self.expr(expr);

        quote! { #expr_tokens #semi }
    }

    pub fn statements(&self, statements: &Vec<Stmt>) -> TokenStream {
        let mut stream = TokenStream::new();
        let mut statement_streams = Vec::<TokenStream>::new();
        let mut loop_match_candidates = Vec::<TokenStream>::new();
        
        for i in 0..statements.len()-1 {
            let stmt = &statements[i];
            
            println!("stmt: {:?}\n", stmt);
            
            match stmt {
                Stmt::Local(ref local) => {
                    if self.config.codeflow && loop_match_candidates.len() > 0 {
                        statement_streams.push(self.engine.loop_match(&loop_match_candidates));
                        loop_match_candidates.clear();
                    }
                    
                    statement_streams.push(self.local(local));
                },
                Stmt::Item(ref item) => {
                    if self.config.codeflow && loop_match_candidates.len() > 0 {
                        statement_streams.push(self.engine.loop_match(&loop_match_candidates));
                        loop_match_candidates.clear();
                    }
                    
                    statement_streams.push(self.item(item));
                },
                Stmt::Expr(ref expr) => {
                    if self.config.codeflow {
                        if self.expr_is_mobile(expr) { loop_match_candidates.push(self.expr(expr)); continue; }
                        else if loop_match_candidates.len() > 0 {
                            statement_streams.push(self.engine.loop_match(&loop_match_candidates));
                            loop_match_candidates.clear();
                        }
                    }

                    statement_streams.push(self.expr(expr));
                },
                Stmt::Semi(ref expr, ref semi) => {
                    if self.config.codeflow {
                        if self.expr_is_mobile(expr) { loop_match_candidates.push(self.semi(expr, semi)); continue; }
                        else if loop_match_candidates.len() > 0 {
                            statement_streams.push(self.engine.loop_match(&loop_match_candidates));
                            loop_match_candidates.clear();
                        }
                    }
                    
                    statement_streams.push(self.semi(expr, semi));
                },
            }
        }

        if self.config.codeflow && loop_match_candidates.len() > 0 {
            statement_streams.push(self.engine.loop_match(&loop_match_candidates));
        }

        let final_statement = &statements[statements.len()-1];
        statement_streams.push(match final_statement {
            Stmt::Local(ref local) => self.local(local),
            Stmt::Item(ref item) => self.item(item),
            Stmt::Expr(ref expr) => self.expr(expr),
            Stmt::Semi(ref expr, ref semi) => self.semi(expr, semi),
        });
        
        stream.extend(statement_streams.into_iter());
        
        stream
    }
}
