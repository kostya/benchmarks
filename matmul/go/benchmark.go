package benchmark

import (
	"benchmarks/common"
	"fmt"
	"math"
)

func verify(calc func(int) float64) error {
	left := calc(101)
	right := -18.67
	if math.Abs(left-right) > 0.1 {
		return fmt.Errorf("%+v != %+v\n", left, right)
	}
	return nil
}

func Run(name string, n int, calc func(int) float64) error {
	err := verify(calc)
	if err != nil {
		return err
	}

	var result float64
	err = common.RunBenchmark(name, func() {
		result = calc(n)
	})
	if err != nil {
		return err
	}

	fmt.Printf("%+v\n", result)
	return nil
}
