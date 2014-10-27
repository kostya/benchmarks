use std::os;

fn newmat(x: uint, y: uint) -> Vec<Vec<f64>> {
  let mut r = Vec::new();
  for _ in range(0, x) {
    let mut c = Vec::new();
    for j in range(0, y) { c.push(0_f64); }
    r.push(c);
  }
  r
}

fn matgen(n: uint) -> Vec<Vec<f64>> {
  let mut a = newmat(n, n);
  let tmp = 1_f64 / (n as f64) / (n as f64);
  for i in range(0, n) {
    for j in range(0, n) {
      let val = tmp * (i as f64 - j as f64) * (i as f64 + j as f64);
      *a.get_mut(i).get_mut(j) = val; // Wtf, rust O_o
    }
  }
  a
}

fn matmul(a: Vec<Vec<f64>>, b: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
  let m = a.len();
  let n = a[0].len();
  let p = b[0].len();

  let mut b2 = newmat(n, p);
  for i in range(0, n) {
    for j in range(0, p) {
      *b2.get_mut(j).get_mut(i) = b[i][j];
    }
  }

  let mut c = newmat(m, p);
  for i in range(0, m) {
    for j in range(0, p) {
      let mut s = 0_f64;
      let ref ai = a[i];
      let ref b2j = b2[j];
      for k in range(0, n) {
        s += ai[k] * b2j[k];
      }
      *c.get_mut(i).get_mut(j) = s;
    }
  }

  c
}

fn main() {
  let args = os::args();
  let mut n = 100;
  if args.len() > 1 { n = from_str(args[1].as_slice()).unwrap(); }
  n = n / 2 * 2;

  let a = matgen(n);
  let b = matgen(n);
  let c = matmul(a, b);
  print!("{}\n", c[n / 2][n / 2]);
}
