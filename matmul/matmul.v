import os
import math
import net

fn make_matrix(n int, m int) [][]f64 {
	mut res := [[f64(0.0)].repeat(m)]
	for i := 1; i < n; i++ {
		res << [f64(0.0)].repeat(m)
	}
	return res
}

fn matgen(n int, seed f64) [][]f64 {
	mut a := make_matrix(n, n)
	tmp := seed / n / n
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			v := tmp * f64(i - j) * f64(i + j)
			a[i][j] = v
		}
	}
	return a
}

fn matmul(a [][]f64, b [][]f64) [][]f64 {
	m := a.len
	n := a[0].len
	p := b[0].len
	// transpose
	mut b2 := make_matrix(n, p)
	for i := 0; i < n; i++ {
		for j := 0; j < p; j++ {
			b2[j][i] = b[i][j]
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
			c[i][j] = s
		}
	}
	return c
}

fn notify(msg string) {
	mut sock := net.dial_tcp('127.0.0.1:9001') or { return }
	defer {
		sock.close() or {}
	}
	sock.write_string(msg) or {}
}

fn calc(n int) f64 {
	size := n / 2 * 2
	a := matgen(size, 1.0)
	b := matgen(size, 2.0)
	c := matmul(a, b)
	return c[size / 2][size / 2]
}

fn main() {
	n := if os.args.len > 1 { os.args[1].int() } else { 100 }
	left := calc(101)
	right := -18.67
	if math.abs(left - right) > 0.1 {
		panic('$left != $right')
	}
	mut lang := 'V/gcc'
	$if clang {
		lang = 'V/clang'
	}
	notify('$lang\t$C.getpid()')
	results := calc(n)
	notify('stop')
	println(results)
}
