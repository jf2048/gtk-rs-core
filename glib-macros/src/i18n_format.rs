use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use std::collections::{BTreeSet, HashMap, HashSet};
use syn::ext::IdentExt;

pub fn impl_1i8n_format(stream: syn::parse::ParseStream) -> syn::Result<TokenStream> {
    let crate_ident = stream.parse::<syn::Path>()?;
    stream.parse::<syn::Token![,]>()?;
    let target = stream.parse::<syn::Expr>()?;
    stream.parse::<syn::Token![,]>()?;
    let fmt = stream.parse::<syn::LitStr>()?;
    let fmt = fmt.value();
    let mut arg_count = 0usize;
    let mut pos_args = Vec::new();
    let mut named_args = HashMap::new();
    while !stream.is_empty() {
        stream.parse::<syn::Token![,]>()?;
        if stream.is_empty() {
            break;
        }
        let ident = format_ident!("arg{}", arg_count, span = Span::mixed_site());
        if !named_args.is_empty()
            || (stream.peek(syn::Ident::peek_any) && stream.peek2(syn::Token![=]))
        {
            let name = stream.call(syn::Ident::parse_any)?;
            stream.parse::<syn::Token![=]>()?;
            let expr = stream.parse::<syn::Expr>()?;
            named_args.insert(name, (ident, expr));
        } else {
            let expr = stream.parse::<syn::Expr>()?;
            pos_args.push((ident, expr));
        }
        arg_count += 1;
    }
    let arg_lets = pos_args
        .iter()
        .chain(named_args.values())
        .map(|(ident, expr)| {
            quote! { let #ident = #expr; }
        });
    let parser =
        parse_format::Parser::new(&fmt, None, None, false, parse_format::ParseMode::Format);
    let printers = parser.filter_map(|piece| {
        use parse_format::{Count::*, Position::*};

        let mut arg = match piece {
            parse_format::NextArgument(arg) => arg,
            _ => return None,
        };
        let mut positions = BTreeSet::new();
        let mut names = HashSet::new();
        let mut args = Vec::new();
        match &arg.position {
            ArgumentIs(index) | ArgumentImplicitlyIs(index) => {
                positions.insert(*index);
            }
            ArgumentNamed(name) => {
                names.insert(syn::Ident::new(*name, Span::mixed_site()));
            }
        };
        match &arg.format.precision {
            CountIs(index) | CountIsParam(index) | CountIsStar(index) => {
                positions.insert(*index);
            }
            CountIsName(name, _) => {
                names.insert(syn::Ident::new(*name, Span::mixed_site()));
            }
            _ => {}
        }
        match &arg.format.width {
            CountIs(index) | CountIsParam(index) | CountIsStar(index) => {
                positions.insert(*index);
            }
            CountIsName(name, _) => {
                names.insert(syn::Ident::new(*name, Span::mixed_site()));
            }
            _ => {}
        }
        let positions = positions.into_iter().enumerate().collect::<Vec<_>>();
        for (to, from) in positions {
            match arg.position {
                ArgumentIs(index) | ArgumentImplicitlyIs(index) => {
                    if index == from {
                        arg.position = ArgumentIs(to);
                    }
                }
                _ => {}
            };
            match arg.format.precision {
                CountIs(index) | CountIsParam(index) | CountIsStar(index) => {
                    if index == from {
                        arg.format.precision = CountIs(to);
                    }
                }
                _ => {}
            }
            match arg.format.width {
                CountIs(index) | CountIsParam(index) | CountIsStar(index) => {
                    if index == from {
                        arg.format.width = CountIs(to);
                    }
                }
                _ => {}
            }
            let ident = &pos_args[from].0;
            args.push(quote! { #ident });
        }
        for name in names {
            if let Some((ident, _)) = named_args.get(&name) {
                args.push(quote! { #name = #ident });
            }
        }
        let fmt = arg.to_string();
        Some(quote! {
            |s: &mut dyn ::std::fmt::Write| -> () {
                ::std::write!(s, #fmt, #(#args),*).unwrap();
            }
        })
    });
    Ok(quote_spanned! { Span::mixed_site() => {
        #(#arg_lets)*
        #crate_ident::replace_i18n_args(#target, [#(&#printers as _),*])
    } })
}
