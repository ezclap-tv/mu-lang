#![no_main]
use libfuzzer_sys::fuzz_target;
use syntax::parser::parse;

fuzz_target!(|data: &[u8]| {
  if let Ok(s) = std::str::from_utf8(data) {
    match parse(&s) {
      Ok(_) => (),
      Err(errors) => {
        let mut buf = String::new();
        let _reports = errors
          .iter()
          .map(|e| diagnosis::ToReport::to_report(e, s.into()).unwrap())
          .for_each(|r| r.emit(&mut buf).unwrap());
      }
    }
  }
});
