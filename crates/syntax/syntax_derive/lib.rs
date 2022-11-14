use derive_utils::quick_derive;
use proc_macro::TokenStream;

#[proc_macro_derive(DebugInner)]
pub fn derive_debug(input: TokenStream) -> TokenStream {
  quick_derive! {
      input,
      std::fmt::Debug,
      trait Debug {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
      }
  }
}
