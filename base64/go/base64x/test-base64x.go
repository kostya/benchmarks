package main

import (
	"benchmark"
	"github.com/chenzhuoyu/base64x"
)

func main() {
	err := benchmark.Run("base64x", base64x.StdEncoding)
	if err != nil {
		panic(err)
	}
}
