extern crate time;
extern crate base64;

use base64::{encode, decode};
use time::precise_time_ns;
use std::str;

const STR_SIZE: usize = 131072;
const TRIES: usize = 8192;

fn main() {
  let input = vec![b'a'; STR_SIZE];

  let mut output = encode(&input);
  print!("encode {}... to {}...: ", str::from_utf8(&input[..4]).unwrap(), &output[..4]);

  let mut time_start = precise_time_ns();
  let mut sum = 0;
  for _ in 0..TRIES {
    output = encode(&input);
    sum += output.len();
  }
  println!("{}, {}", sum, ((precise_time_ns() - time_start) as f64) / 1e9);

  let mut str3 = decode(&output).unwrap();
  print!("decode {}... to {}...: ", &output[..4], str::from_utf8(&str3[..4]).unwrap());
  sum = 0;
  time_start = precise_time_ns();
  for _ in 0..TRIES {
    str3 = decode(&output).unwrap();
    sum += str3.len();
  }
  println!("{}, {}", sum, ((precise_time_ns() - time_start) as f64) / 1e9);
}

