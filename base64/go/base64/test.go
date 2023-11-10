package main

import (
	"benchmark"
	"encoding/base64"
)

func main() {
	err := benchmark.Run("", base64.StdEncoding)
	if err != nil {
		panic(err)
	}
}
