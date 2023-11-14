package benchmark

import (
	"benchmarks/common"
	"fmt"
	"slices"
)

type benchmarkFunc func(upperBound, prefix int) []int

func verify(bench benchmarkFunc) error {
	left := []int{2, 23, 29}
	right := bench(100, 2)
	if !slices.Equal(left, right) {
		return fmt.Errorf("%+v != %+v\n", left, right)
	}
	return nil
}

func Run(name string, bench benchmarkFunc) error {
	const (
		upperBound = 5_000_000
		prefix     = 32_338
	)

	err := verify(bench)
	if err != nil {
		return err
	}

	var result []int
	err = common.RunBenchmark(name, func() {
		result = bench(upperBound, prefix)
	})
	if err != nil {
		return err
	}

	fmt.Printf("%+v\n", result)
	return nil
}
