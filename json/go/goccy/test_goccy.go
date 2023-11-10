package main

import (
	"benchmark"
	"github.com/goccy/go-json"
)

func calc(bytes []byte) (benchmark.Coordinate, error) {
	obj := benchmark.TestStruct{}
	err := json.Unmarshal(bytes, &obj)
	if err != nil {
		return benchmark.Coordinate{}, err
	}
	return obj.Average(), nil
}

func main() {
	err := benchmark.Run("goccy/go-json", calc)
	if err != nil {
		panic(err)
	}
}
