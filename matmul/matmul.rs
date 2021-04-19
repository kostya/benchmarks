fn new_mat(x: usize, y: usize) -> Vec<Vec<f64>> {
    vec![vec![0f64; y]; x]
}

fn mat_gen(n: usize, seed: f64) -> Vec<Vec<f64>> {
    let mut m = new_mat(n, n);
    let tmp = seed / (n as f64) / (n as f64);

    for (i, row) in m.iter_mut().enumerate() {
        for (j, x) in row.iter_mut().enumerate() {
            *x = tmp * (i as f64 - j as f64) * (i as f64 + j as f64);
        }
    }
    m
}

fn mat_mul(a: &[Vec<f64>], b: &[Vec<f64>]) -> Vec<Vec<f64>> {
    let m = a.len();
    let n = a[0].len();
    let p = b[0].len();

    let mut b2 = new_mat(n, p);
    for (i, row) in b.into_iter().enumerate() {
        for (j, x) in row.into_iter().enumerate() {
            b2[j][i] = *x;
        }
    }

    let mut c = new_mat(m, p);

    for (ci, a_row) in c.iter_mut().zip(a.into_iter()) {
        for (cij, b2j) in ci.iter_mut().zip(&b2) {
            *cij = a_row.iter().zip(b2j).fold(0f64, |acc, (&x, y)| acc + x * y);
        }
    }

    c
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).ok();
    }
}

fn calc(n: usize) -> f64 {
    let size = n / 2 * 2;
    let a = mat_gen(size, 1.0);
    let b = mat_gen(size, 2.0);
    let c = mat_mul(&a, &b);
    c[size / 2][size / 2]
}

fn main() {
    let n = std::env::args()
        .nth(1)
        .unwrap_or("100".into())
        .parse::<usize>()
        .unwrap();

    let left = calc(101);
    let right = -18.67;
    if (left - right).abs() > 0.1 {
        eprintln!("{} != {}", left, right);
        std::process::exit(-1);
    }

    notify(&format!("Rust\t{}", std::process::id()));
    let results = calc(n);
    notify("stop");

    println!("{}", results);
}
