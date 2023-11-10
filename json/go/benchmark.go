package benchmark

import (
	"fmt"
	"os"

	"benchmarks/common"
)

type Coordinate struct {
	X, Y, Z float64
}

type TestStruct struct {
	Coordinates []Coordinate
}

func (t TestStruct) Average() Coordinate {
	var coord Coordinate
	for i := range t.Coordinates {
		coord.X += t.Coordinates[i].X
		coord.Y += t.Coordinates[i].Y
		coord.Z += t.Coordinates[i].Z
	}
	count := float64(len(t.Coordinates))
	coord.X /= count
	coord.Y /= count
	coord.Z /= count
	return coord
}

func verify(calc func([]byte) (Coordinate, error)) error {
	right := Coordinate{2.0, 0.5, 0.25}
	for _, v := range []string{
		`{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
		`{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`} {
		left, err := calc([]byte(v))
		if err != nil {
			return err
		}
		if left != right {
			return fmt.Errorf("%+v != %+v\n", left, right)
		}
	}
	return nil
}

func Run(name string, calc func([]byte) (Coordinate, error)) error {
	err := verify(calc)
	if err != nil {
		return err
	}

	data, err := os.ReadFile("/tmp/1.json")
	if err != nil {
		return err
	}
	var result Coordinate
	var calcErr error
	err = common.RunBenchmark(name, func() {
		result, calcErr = calc(data)
	})
	if err != nil {
		return err
	}
	if calcErr != nil {
		return calcErr
	}
	fmt.Printf("%+v\n", result)
	return nil
}
