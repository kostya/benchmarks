// rustc -C opt-level=3 -C lto

fn new_mat(x: usize, y: usize) -> Vec<Vec<f64>> {
    vec![vec![0f64; y]; x]
}

fn mat_gen(n: usize) -> Vec<Vec<f64>> {
    let mut m = new_mat(n, n);
    let tmp = 1f64 / (n as f64) / (n as f64);

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

fn main() {
    {
        use std::io::Write;
        if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
            stream.write_all(b"Rust").unwrap();
        }
    }

    let n = std::env::args()
            .nth(1)
            .unwrap_or("1500".into())
            .parse::<usize>()
            .unwrap() / 2 * 2;

    let a = mat_gen(n);
    let b = mat_gen(n);
    let c = mat_mul(&a, &b);

    println!("{}", c[n / 2][n / 2]);
}
