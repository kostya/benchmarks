extern crate rustc_serialize;
use rustc_serialize::base64::{ToBase64, FromBase64, STANDARD};
extern crate time;
use time::precise_time_ns;

const STR_SIZE: usize = 10000000;
const TRIES: usize = 100;

fn main() {
  let input = vec![b'a'; STR_SIZE];
  let mut output = String::new();

  let mut time_start = precise_time_ns();
  let mut sum = 0;
  for _ in 0..TRIES {
    output = input.to_base64(STANDARD);
    sum += output.len();
  }
  println!("encode: {}, {}", sum, ((precise_time_ns() - time_start) as f64) / 1e9);

  sum = 0;
  time_start = precise_time_ns();
  for _ in 0..TRIES {
    sum += output.from_base64().unwrap().len();
  }
  println!("decode: {}, {}", sum, ((precise_time_ns() - time_start) as f64) / 1e9);
}

