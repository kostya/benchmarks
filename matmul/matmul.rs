// rustc -C opt-level=3 -C lto

fn new_mat(x: usize, y: usize) -> Vec<Vec<f64>> {
    vec![vec![0f64; y]; x]
}

fn mat_gen(n: usize, seed: f64) -> Vec<Vec<f64>> {
    let mut m = new_mat(n, n);
    let tmp = seed / (n as f64) / (n as f64);

    for i in 0 .. n {
        for j in 0 .. n {
            m[i][j] = tmp * (i as f64 - j as f64) * (i as f64 + j as f64);
        }
    }
    m
}

#[inline(never)]
fn mat_mul(a: &[Vec<f64>], b: &[Vec<f64>]) -> Vec<Vec<f64>> {
    let m = a.len();
    let n = a[0].len();
    let p = b[0].len();

    let mut b2 = new_mat(n, p);
    for i in 0 .. n {
        for j in 0 .. p {
            b2[j][i] = b[i][j];
        }
    }

    let mut c = new_mat(m, p);

    for (i, ci) in c.iter_mut().enumerate() {
        for (cij, b2j) in ci.iter_mut().zip(&b2) {
            *cij = a[i].iter().zip(b2j).map(|(&x, y)| x * y).sum();
        }
    }

    c
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
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
