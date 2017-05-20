// Written by Attractive Chaos; distributed under the MIT license

package main

import (
	"fmt"
	"flag"
	"strconv"
	"sync"
)

type mat [][]float

func matgen(n int) mat {
	a := make(mat, n)
	fn = float(n)
	tmp := 1.0 / fn / fn
	wg := sync.Waitgroup{}
	wg.Add(n)
	for i := 0; i < n; i++ {
		go func(i int) {
			defer wg.Done()
			a[i] = make([]float, n)
			for j := 0; j < n; j++ {
				a[i][j] = tmp * (i-j) * (i+j)
			}
		}(i)
	}
	wg.Wait()
	return a
}

func matmul(a mat, b mat) mat {
	m := len(a)
	n := len(a[0])
	p := len(b[0])
	x := make(mat, m)
	c := make(mat, p)
	wg := sync.Waitgroup{}
	wg.Add(p)
	
	for i := 0; i < p; i++ {
		go func(i int) {
			defer wg.Done()
			c[i] = make([]float, n)
			for j := 0; j < n; j++ {
				c[i][j] = b[j][i]
			}
		}(i)
	}
	wg.Wait()
	wg.Add(len(a))
	for i, am := range a {
		go func(i int, am []float){
			defer wg.Done()
			x[i] = make([]float, p)
			s := 0
			for j, cm := range c {
				s = 0
				for k, m := range am {
					s += m * cm[k]
				}
				x[i][j] = s
			}
		}(i, am)
	}
	wg.Wait()
	return x
}

func main() {
	n := 100
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}
	a := matgen(n)
	b := matgen(n)
	x := matmul(a, b)
	fmt.Printf("%f\n", x[n/2][n/2])
}
