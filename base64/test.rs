extern crate time;
extern crate serialize;
use serialize::base64::{ToBase64, STANDARD, FromBase64};
use std::string::String;
use time::precise_time_ns;

#[warn(non_snake_case)]

fn main () {
  let str_size = 10000000u;
  let tries = 100u;

  let mut str: String = "".to_string();
  let mut str2: String = "".to_string();
  for _ in range(0u, str_size) { str.push_str("a"); }
  let bytes = str.as_bytes();
  let slice = bytes.as_slice();

  let mut t = precise_time_ns();
  let mut s = 0u;

  for _ in range(0u, tries) {
    str2 = slice.to_base64(STANDARD);
    s += str2.len();
  }
  println!("encode: {}, {}", s, ((precise_time_ns() - t) as f64) / 1e9);

  let str2_slice = str2.as_slice();

  s = 0;
  t = precise_time_ns();
  for _ in range(0u, tries) {
    s += str2_slice.from_base64().unwrap().len();
  }

  println!("decode: {}, {}", s, ((precise_time_ns() - t) as f64) / 1e9);
}

