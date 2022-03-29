use ndarray::{linalg::general_mat_mul, Array, Array2};
use std::io::Write;
use std::net::TcpStream;
use std::{env, process};

fn mat_gen(n: usize, seed: f64) -> Array2<f64> {
    let i = Array::from_shape_fn((n, n), |(i, _)| i as f64);
    let j = i.t();

    let n = n as f64;
    (&i - &j) * (&i + &j) * (seed / n / n)
}

fn mat_mul(a: &Array2<f64>, b: &Array2<f64>) -> Array2<f64> {
    let mut c = Array2::<f64>::zeros((b.shape()[0], a.shape()[1]));
    general_mat_mul(1., a, b, 1., &mut c);
    c
}

fn notify(msg: &str) {
    if let Ok(mut stream) = TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(n: usize) -> f64 {
    let size = n / 2 * 2;
    let a = mat_gen(size, 1.0);
    let b = mat_gen(size, 2.0);
    let c = mat_mul(&a, &b);
    c[(size / 2, size / 2)]
}

fn main() {
    let n = env::args()
        .nth(1)
        .unwrap_or_else(|| "100".into())
        .parse::<usize>()
        .unwrap();

    let left = calc(101);
    let right = -18.67;
    if (left - right).abs() > 0.1 {
        eprintln!("{} != {}", left, right);
        process::exit(-1);
    }

    notify(&format!("Rust\t{}", process::id()));
    let results = calc(n);
    notify("stop");

    println!("{}", results);
}
