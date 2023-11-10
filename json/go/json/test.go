package main

import (
	"benchmark"
	"encoding/json"
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
	err := benchmark.Run("", calc)
	if err != nil {
		panic(err)
	}
}
