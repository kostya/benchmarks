package benchmark

import (
	"benchmarks/common"
	"fmt"
	"os"
)

type Printer interface {
	Reset(quiet bool)
	Print(int)
	GetChecksum() int
}

type runFunc func(string, Printer)

func verify(runProgram runFunc, printer Printer) error {
	const input = `++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.`
	const want = "Hello World!\n"

	printer.Reset(true)
	runProgram(input, printer)
	left := printer.GetChecksum()

	printer.Reset(true)
	for _, c := range want {
		printer.Print(int(c))
	}
	right := printer.GetChecksum()

	if left != right {
		return fmt.Errorf("%+v != %+v\n", left, right)
	}
	return nil
}

func Run(runProgram runFunc, printer Printer) error {
	err := verify(runProgram, printer)
	if err != nil {
		return err
	}

	code, err := os.ReadFile(os.Args[1])
	if err != nil {
		return err
	}
	text := string(code)

	_, quiet := os.LookupEnv("QUIET")
	printer.Reset(quiet)

	err = common.RunBenchmark("", func() {
		runProgram(text, printer)
	})
	if err != nil {
		return err
	}

	if quiet {
		fmt.Printf("Output checksum: %d\n", printer.GetChecksum())
	}
	return nil
}
