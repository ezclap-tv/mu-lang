#![feature(proc_macro_diagnostic)]
extern crate syn;
#[macro_use]
extern crate quote;

use std::path::PathBuf;

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::Ident;

#[proc_macro_attribute]
pub fn mu_test_suite(_: TokenStream, item: TokenStream) -> TokenStream {
  let mut module = syn::parse::<syn::ItemMod>(item).expect("This macro must be used on a module.");

  let suite_root = ::std::env::var("CARGO_MANIFEST_DIR").unwrap();
  for item in module
    .content
    .as_mut()
    .iter_mut()
    .flat_map(|(_, items)| items)
  {
    match check_attrs(item) {
      Ok(Some(attr)) => match attr {
        SuiteItemKind::SuiteRoot(_) => todo!(),
        SuiteItemKind::TestsNamespace(path) => {
          let namespace = match &path {
            syn::Lit::Str(s) => s.value(),
            _ => {
              return syn::Error::new(path.span(), "The path literal must be a string")
                .into_compile_error()
                .into()
            }
          };

          let tests_dir = PathBuf::from(suite_root.clone())
            .join(namespace.replace('/', &std::path::MAIN_SEPARATOR.to_string()[..]));

          if !tests_dir.exists() {
            return syn::Error::new(
              path.span(),
              format!("Couldn't find the directory `{}`", tests_dir.display()),
            )
            .into_compile_error()
            .into();
          }

          let r#mod = match item {
            syn::Item::Mod(r#mod) if r#mod.content.is_some() => r#mod,
            _ => continue,
          };

          let mut callbacks = Callbacks {
            ok: None,
            err: None,
            input: None,
          };

          macro_rules! check_used_once {
            ($ident:expr, $other:expr) => {
              match check_used_once($ident, $other) {
                Ok(ok) => Some(ok),
                Err(e) => return e,
              }
            };
          }

          macro_rules! append {
            ($content:expr, $($code:tt)*) => {
                $content.push(
                    syn::parse::<syn::Item>(
                        quote! {
                            $($code)*
                        }
                        .into(),
                    )
                    .unwrap(),
                )
            };
          }

          for item in &mut r#mod
            .content
            .as_mut()
            .iter_mut()
            .flat_map(|(_, items)| items)
          {
            match check_attrs(item) {
              Ok(Some(attr)) => match attr {
                SuiteItemKind::OkCallback(ok) => {
                  callbacks.ok = check_used_once!(&callbacks.ok, ok);
                }
                SuiteItemKind::ErrCallback(err) => {
                  callbacks.err = check_used_once!(&callbacks.err, err);
                }
                SuiteItemKind::InputCallback(input) => {
                  callbacks.input = check_used_once!(&callbacks.err, input);
                }
                _ => (),
              },
              Err(e) => {
                return e.into_compile_error().into();
              }
              Ok(None) => (),
            }
          }

          if callbacks.ok.is_none() {
            return syn::Error::new(
              path.span(),
              "The namespace must define at least one callback",
            )
            .into_compile_error()
            .into();
          }

          let tests = match std::fs::read_dir(tests_dir) {
            Ok(files) => files
              .filter_map(|f| f.ok())
              .filter_map(|e| {
                if e
                  .path()
                  .extension()
                  .map(|ext| ext == "test")
                  .unwrap_or(false)
                {
                  Some(Ident::new(
                    &e.path().file_stem().unwrap().to_string_lossy()[..],
                    path.span(),
                  ))
                } else {
                  None
                }
              })
              .collect::<Vec<_>>(),
            Err(e) => {
              return syn::Error::new(path.span(), format!("Failed to read the test files: {}", e))
                .into_compile_error()
                .into();
            }
          };

          let content = &mut r#mod.content.as_mut().unwrap().1;

          append!(content, static __gen_CRATE_ROOT: &str = #suite_root;);
          append!(content, static __gen_TESTS_DIR: &str = #namespace;);

          let ok = callbacks.ok.unwrap();
          let mut macro_name = Ident::new("test_eq", ok.span());
          if callbacks.err.is_some() {
            let err = callbacks.err.unwrap();
            macro_name = Ident::new("test_auto", ok.span());
            if callbacks.input.is_some() {
              let input = callbacks.input.unwrap();
              append!(content,
                  mu_testing::make_test_macros!(__gen_CRATE_ROOT, __gen_TESTS_DIR, #ok, #err, #input);
              );
            } else {
              append!(content,
                  mu_testing::make_test_macros!(__gen_CRATE_ROOT, __gen_TESTS_DIR, #ok, #err);
              );
            }
          } else if callbacks.input.is_some() {
            let input = callbacks.input.unwrap();
            append!(content,
                mu_testing::make_test_macros!(eq => __gen_CRATE_ROOT, __gen_TESTS_DIR, #ok, #input);
            );
          } else {
            append!(content,
                mu_testing::make_test_macros!(eq => __gen_CRATE_ROOT, __gen_TESTS_DIR, #ok);
            );
          }

          for test in tests {
            append!(content, #macro_name!(#test););
          }
        }
        _ => {
          return syn::Error::new(
            item.span(),
            "The suite items may specify only the mu_tests and mu_test_root attributes.",
          )
          .into_compile_error()
          .into()
        }
      },
      Err(e) => {
        return e.into_compile_error().into();
      }
      Ok(None) => (),
    }
  }

  module.into_token_stream().into()
}

fn check_used_once(ident: &Option<Ident>, other: Ident) -> Result<Ident, TokenStream> {
  if ident.is_some() {
    Err(
      syn::Error::new(
        other.span(),
        "Only a single ok/err/input callback attribute maybe used per callback per namespace.",
      )
      .into_compile_error()
      .into(),
    )
  } else {
    Ok(other)
  }
}

struct Callbacks {
  ok: Option<Ident>,
  err: Option<Ident>,
  input: Option<Ident>,
}

enum SuiteItemKind {
  SuiteRoot(Ident),
  TestsNamespace(syn::Lit),
  OkCallback(Ident),
  ErrCallback(Ident),
  InputCallback(Ident),
}

fn check_attrs(item: &mut syn::Item) -> Result<Option<SuiteItemKind>, syn::Error> {
  Ok(match item {
    syn::Item::Static(st) => match has_attr(&st.attrs, "mu_suite_root") {
      Some(idx) => {
        st.attrs.remove(idx);
        let _ = Some(SuiteItemKind::SuiteRoot(st.ident.clone()));
        unimplemented!();
      }
      _ => None,
    },
    syn::Item::Mod(module) => match has_attr(&module.attrs, "mu_tests") {
      Some(idx) => {
        let attr = module.attrs.remove(idx);
        let lit = parse_key_value_attr(&attr)?;
        Some(SuiteItemKind::TestsNamespace(lit))
      }
      _ => None,
    },
    syn::Item::Use(r#use) => {
      let mut tree = &r#use.tree;
      while let syn::UseTree::Path(path) = tree {
        tree = &path.tree;
      }
      let name = match &tree {
                syn::UseTree::Name(path) => {
                    Ok(path.ident.clone())
                }
                syn::UseTree::Rename(path) =>{
                    Ok(path.rename.clone())
                }
                _ => Err(syn::Error::new(r#use.span(), "The callback attributes may only be used on functions and use items that import functions")),
            };
      if let Some(idx) =
        has_attr(&r#use.attrs, "ok_callback").or_else(|| has_attr(&r#use.attrs, "eq_callback"))
      {
        r#use.attrs.remove(idx);
        Some(SuiteItemKind::OkCallback(name?))
      } else if let Some(idx) = has_attr(&r#use.attrs, "err_callback") {
        r#use.attrs.remove(idx);
        Some(SuiteItemKind::ErrCallback(name?))
      } else if let Some(idx) = has_attr(&r#use.attrs, "input_callback") {
        r#use.attrs.remove(idx);
        Some(SuiteItemKind::InputCallback(name?))
      } else {
        None
      }
    }
    syn::Item::Fn(r#fn) => {
      if let Some(idx) =
        has_attr(&r#fn.attrs, "ok_callback").or_else(|| has_attr(&r#fn.attrs, "eq_callback"))
      {
        r#fn.attrs.remove(idx);
        Some(SuiteItemKind::OkCallback(r#fn.sig.ident.clone()))
      } else if let Some(idx) = has_attr(&r#fn.attrs, "err_callback") {
        r#fn.attrs.remove(idx);
        Some(SuiteItemKind::ErrCallback(r#fn.sig.ident.clone()))
      } else if let Some(idx) = has_attr(&r#fn.attrs, "input_callback") {
        r#fn.attrs.remove(idx);
        Some(SuiteItemKind::InputCallback(r#fn.sig.ident.clone()))
      } else {
        None
      }
    }
    _ => None,
  })
}

fn has_attr(attrs: &[syn::Attribute], attr_name: &str) -> Option<usize> {
  attrs
    .iter()
    .enumerate()
    .find(|(_, attr)| attr.path.is_ident(attr_name))
    .map(|(idx, _)| idx)
}

/// Extracts the string literal from an attribute in the form `#[name = "value"]`.
fn parse_key_value_attr(attr: &syn::Attribute) -> Result<syn::Lit, syn::Error> {
  Ok(
    match attr
      .parse_meta()
      .expect("Failed to parse the contents of the attribute")
    {
      syn::Meta::NameValue(meta) => match meta.lit {
        syn::Lit::Str(_) => meta.lit,
        _ => {
          return Err(syn::Error::new(
            meta.span(),
            "The default value must be a string literal",
          ))
        }
      },
      rest => {
        return Err(syn::Error::new(
          rest.span(),
          "Only the key-value syntax is supported: `#[key = \"a value\"]`",
        ))
      }
    },
  )
}
