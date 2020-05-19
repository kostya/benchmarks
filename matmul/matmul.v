import os
import math
import net

fn make_matrix(n int, m int) [] []f64 {
  mut res := [[f64(0.0)].repeat(m)]
  for i := 1; i < n; i++ {
    res << [f64(0.0)].repeat(m)
  }
  return res
}

fn matgen(n int) [] []f64 {
  mut a := make_matrix(n, n)
  tmp := f64(1.0) / n / n

  for i := 0; i < n; i++ {
    for j := 0; j < n; j++ {
      v := tmp * f64(i - j) * f64(i + j)
      a[i] [j] = v
    }
  }

  return a
}

fn matmul(a [] []f64, b [] []f64) [] []f64 {
  m := a.len
  n := a[0].len
  p := b[0].len

  // transpose
  mut b2 := make_matrix(n, p)
  for i := 0; i < n; i++ {
    for j := 0; j < p; j++ {
      b2[j] [i] = b[i] [j]
    }
  }

  // mul
  mut c := make_matrix(m, p)  
  for i := 0; i < m; i++ {
    for j := 0; j < p; j++ {
      mut s := 0.0
      ai := a[i]
      b2j := b2[j]

      for k := 0; k < n; k++ {
        s += ai[k] * b2j[k]
      }

      c[i] [j] = s
    }
  }

  return c
}

fn notify(msg string) {
    sock := net.dial('127.0.0.1', 9001) or {
        return
    }
    sock.write(msg) or {}
    sock.close() or {}
}

fn main() {
    n := if os.args.len > 1 {
        os.args[1].int() / 2 * 2
    } else {
        100
    }

    t := matmul(matgen(100), matgen(100))
    if math.abs(t[1][1] + 19.5) > 0.5 {
      exit(-1)
    }

    mut lang := "V GCC"
    $if clang {
        lang = "V Clang"
    }
    notify('${lang}\t${C.getpid()}')

    a := matgen(n)
    b := matgen(n)
    c := matmul(a, b)
    println(c[n / 2] [n / 2])

    notify("stop")
}
