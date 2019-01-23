extern crate base64;
extern crate time;

use base64::{decode, encode};
use time::precise_time_ns;

const STR_SIZE: usize = 10_000_000;
const TRIES: usize = 100;

fn main() {
    let input = vec![b'a'; STR_SIZE];
    let mut output = String::new();

    let time_start = precise_time_ns();
    let mut sum = 0;
    for _ in 0..TRIES {
        output = encode(&input);
        sum += output.len();
    }
    println!(
        "encode: {}, {}",
        sum,
        ((precise_time_ns() - time_start) as f64) / 1e9
    );

    let mut sum = 0;
    let time_start = precise_time_ns();
    for _ in 0..TRIES {
        sum += decode(&output).unwrap().len();
    }
    println!(
        "decode: {}, {}",
        sum,
        ((precise_time_ns() - time_start) as f64) / 1e9
    );
}
