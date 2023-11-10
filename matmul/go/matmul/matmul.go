// Written by Attractive Chaos; distributed under the MIT license

package main

import (
	"benchmark"
	"os"
	"strconv"
)

func matgen(n int, seed float64) [][]float64 {
	a := make([][]float64, n)
	tmp := seed / float64(n) / float64(n) // pretty silly...
	for i := 0; i < n; i++ {
		a[i] = make([]float64, n)
		for j := 0; j < n; j++ {
			a[i][j] = tmp * float64(i-j) * float64(i+j)
		}
	}
	return a
}

func matmul(a [][]float64, b [][]float64) [][]float64 {
	m := len(a)
	n := len(a[0])
	p := len(b[0])
	x := make([][]float64, m)
	c := make([][]float64, p)
	for i := 0; i < p; i++ {
		c[i] = make([]float64, n)
		for j := 0; j < n; j++ {
			c[i][j] = b[j][i]
		}
	}
	for i, am := range a {
		x[i] = make([]float64, p)
		for j, cm := range c {
			s := float64(0)
			for k, m := range am {
				s += m * cm[k]
			}
			x[i][j] = s
		}
	}
	return x
}

func calc(n int) float64 {
	n = n / 2 * 2
	a := matgen(n, 1.0)
	b := matgen(n, 2.0)
	x := matmul(a, b)
	return x[n/2][n/2]
}

func main() {
	n := 100
	var err error
	if len(os.Args) > 1 {
		n, err = strconv.Atoi(os.Args[1])
		if err != nil {
			panic(err)
		}
	}

	err = benchmark.Run("", n, calc)
	if err != nil {
		panic(err)
	}
}
