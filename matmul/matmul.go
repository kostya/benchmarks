// Written by Attractive Chaos; distributed under the MIT license

package main

import (
	"flag"
	"fmt"
	"log"
	"math"
	"net"
	"os"
	"runtime"
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
	c := make([][]float64, m)
	for i := 0; i < m; i++ {
		c[i] = make([]float64, p)
		for j := 0; j < p; j++ {
			for k := 0; k < n; k++ {
				c[i][j] += a[i][k] * b[k][j]
			}
		}
	}
	return c
}

func notify(msg string) {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, msg)
		conn.Close()
	}
}

func calc(n int) float64 {
	n = n / 2 * 2
	a := matgen(n, 1.0)
	b := matgen(n, 2.0)
	x := matmul(a, b)
	return x[n/2][n/2]
}

func main() {
	n := int(100)
	flag.Parse()
	if flag.NArg() > 0 {
		n, _ = strconv.Atoi(flag.Arg(0))
	}

	left := calc(101)
	right := -18.67
	if math.Abs(left-right) > 0.1 {
		log.Fatalf("%f != %f\n", left, right)
	}

	notify(fmt.Sprintf("%s\t%d", runtime.Compiler, os.Getpid()))
	results := calc(n)
	notify("stop")

	fmt.Printf("%f\n", results)
}
