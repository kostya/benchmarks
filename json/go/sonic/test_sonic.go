package main

import (
	"benchmark"
	"github.com/bytedance/sonic"
)

func calc(bytes []byte) (benchmark.Coordinate, error) {
	obj := benchmark.TestStruct{}
	err := sonic.Unmarshal(bytes, &obj)
	if err != nil {
		return benchmark.Coordinate{}, err
	}
	return obj.Average(), nil
}

func main() {
	err := benchmark.Run("Sonic", calc)
	if err != nil {
		panic(err)
	}
}
