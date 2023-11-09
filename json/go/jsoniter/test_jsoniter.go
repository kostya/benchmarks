package main

import (
	"benchmark"
	jsoniter "github.com/json-iterator/go"
)

func calc(bytes []byte) (benchmark.Coordinate, error) {
	var json = jsoniter.ConfigCompatibleWithStandardLibrary

	obj := benchmark.TestStruct{}
	err := json.Unmarshal(bytes, &obj)
	if err != nil {
		return benchmark.Coordinate{}, err
	}
	return obj.Average(), nil
}

func main() {
	err := benchmark.Run("jsoniter", calc)
	if err != nil {
		panic(err)
	}
}
