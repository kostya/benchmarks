use ndarray::{Array, Array2};
use std::io::Write;
use std::{env, process};
use utils::notify;

fn mat_gen(n: usize, seed: f64) -> Array2<f64> {
    let i = Array::from_shape_fn((n, n), |(i, _)| i as f64);
    let j = i.t();

    let n = n as f64;
    (&i - &j) * (&i + &j) * (seed / n / n)
}

fn calc(n: usize) -> f64 {
    let size = n / 2 * 2;
    let a = mat_gen(size, 1.0);
    let b = mat_gen(size, 2.0);
    let c = a.dot(&b);
    c[(size / 2, size / 2)]
}

fn main() {
    let n = env::args()
        .nth(1)
        .and_then(|x| x.parse::<usize>().ok())
        .unwrap_or(100);

    let left = calc(101);
    let right = -18.67;
    assert!((left - right).abs() <= 0.1, "{left} != {right}");

    notify!("Rust (ndarray)\t{pid}", pid = process::id());
    let results = calc(n);
    notify!("stop");

    println!("{results}");
}
