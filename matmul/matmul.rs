fn mat_gen(n: usize) -> Vec<Vec<f64>> {
    let mut m = vec![vec![0.0; n]; n];
    let k = 1.0 / (n as f64) / (n as f64);

    for i in 0 .. n {
        for j in 0 .. n {
            m[i][j] = k * (i as f64 - j as f64) * (i as f64 + j as f64);
        }
    }
    m
}

#[inline(never)]
fn dot_product(a: &[f64], b: &[f64]) -> f64 {
    debug_assert_eq!(a.len(), b.len());
    let size = a.len() as isize;
    let mut ap = a.as_ptr();
    let mut bp = b.as_ptr();
    let mut tot = [0.0, 0.0];

    unsafe {
        let end_ptr = ap.offset(size);
        const BLOCK_SIZE: isize = 16;
        let block_end_ptr = ap.offset(size & !(BLOCK_SIZE - 1));

        while ap != block_end_ptr {
            for i in 0 .. BLOCK_SIZE {
                tot[i as usize % 2] += *ap.offset(i) * *bp.offset(i);
            }
            ap = ap.offset(BLOCK_SIZE);
            bp = bp.offset(BLOCK_SIZE);
        }

        tot[0] += tot[1];

        while ap != end_ptr {
            tot[0] += *ap * *bp;
            ap = ap.offset(1);
            bp = bp.offset(1);
        }
    }

    tot[0]
}

fn mat_mul(a: &[Vec<f64>], b: &[Vec<f64>]) -> Vec<Vec<f64>> {
    let m = a.len();
    let n = a[0].len();
    let p = b[0].len();

    let mut c = vec![vec![0.0; p]; n];
    for (i, b_row) in b.iter().enumerate() {
        for (j, b_el) in b_row.iter().enumerate() {
            c[j][i] = *b_el;
        }
    }

    let mut res = vec![vec![0.0; p]; m];
    for (i, a_row) in a.iter().enumerate() {
        for (res_ij, c_row) in res[i].iter_mut().zip(&c) {
            *res_ij = dot_product(a_row, c_row);
        }
    }

    res
}

fn main() {
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