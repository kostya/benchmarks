import os
import math
import net
import vsl.la

fn matgen(n int, seed f64) &la.Matrix[f64] {
	mut a := []f64{len: n * n}
	tmp := seed / n / n
	for i in 0 .. n {
		for j in 0 .. n {
			v := tmp * f64(i - j) * f64(i + j)
			a[i * n + j] = v
		}
	}
	return la.matrix_raw(n, n, a)
}

[inline]
fn matmul(a &la.Matrix[f64], b &la.Matrix[f64]) &la.Matrix[f64] {
	mut c := la.new_matrix[f64](a.m, b.n)
	la.matrix_matrix_mul(mut c, 1.0, a, b)
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
	return c.get(size / 2, size / 2)
}

fn main() {
	n := if os.args.len > 1 { os.args[1].int() } else { 100 }
	left := calc(101)
	right := -18.67
	if math.abs(left - right) > 0.1 {
		panic('${left} != ${right}')
	}
	mut label := 'VSL'
	mut compiler := 'gcc'
	$if clang {
		compiler = 'clang'
	}
	$if cblas ? {
		label = 'VSL + CBLAS'
	}
	mut lang := 'V/${compiler} (${label})'
	notify('${lang}\t${C.getpid()}')
	results := calc(n)
	notify('stop')
	println(results)
}
