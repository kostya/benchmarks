extern crate time;
extern crate base64;

use time::precise_time_ns;
use base64::{encode, decode};

const STR_SIZE: usize = 10000000;
const TRIES: usize = 100;

fn main() {
  let input = vec![b'a'; STR_SIZE];
  let mut output = String::new();

  let mut time_start = precise_time_ns();
  let mut sum = 0;
  for _ in 0..TRIES {
    output = encode(&input);
    sum += output.len();
  }
  println!("encode: {}, {}", sum, ((precise_time_ns() - time_start) as f64) / 1e9);

  sum = 0;
  time_start = precise_time_ns();
  for _ in 0..TRIES {
    sum += decode(&output).unwrap().len();
  }
  println!("decode: {}, {}", sum, ((precise_time_ns() - time_start) as f64) / 1e9);
}

