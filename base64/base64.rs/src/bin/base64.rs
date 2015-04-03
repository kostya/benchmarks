extern crate rustc_serialize;
use rustc_serialize::base64::{ToBase64, FromBase64, STANDARD};
extern crate time;
use time::precise_time_ns;

fn main() {
  let str_size = 10000000;
  let tries = 100;

  let mut str: String = "".to_string();
  let mut str2: String = "".to_string();
  for _ in 0..str_size { str.push_str("a"); }
  let bytes = str.as_bytes();
  
  let mut t = precise_time_ns();
  let mut s = 0;

  for _ in 0..tries {
    str2 = bytes.to_base64(STANDARD);
    s += str2.len();
  }
  println!("encode: {}, {}", s, ((precise_time_ns() - t) as f64) / 1e9);

  s = 0;
  t = precise_time_ns();
  for _ in 0..tries {
    s += str2.from_base64().unwrap().len();
  }

  println!("decode: {}, {}", s, ((precise_time_ns() - t) as f64) / 1e9);
}

