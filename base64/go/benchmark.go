package benchmark

import (
	"fmt"
	"strings"
	"time"

	"benchmarks/common"
)

const (
	strSize = 131_072
	tries   = 8_192
)

type Encoding interface {
	EncodeToString([]byte) string
	DecodeString(string) ([]byte, error)
}

func verify(encoding Encoding) error {
	for _, fixture := range [][]string{
		{"hello", "aGVsbG8="},
		{"world", "d29ybGQ="},
	} {
		src := fixture[0]
		dst := fixture[1]
		encoded := encoding.EncodeToString([]byte(src))
		if encoded != dst {
			return fmt.Errorf("%+v != %+v\n", encoded, dst)
		}
		decoded, err := encoding.DecodeString(dst)
		if err != nil {
			return err
		}
		if string(decoded) != src {
			return fmt.Errorf("%+v != %+v\n", decoded, src)
		}
	}
	return nil
}

func Run(name string, encoding Encoding) error {
	err := verify(encoding)
	if err != nil {
		return err
	}

	bytes := []byte(strings.Repeat("a", strSize))
	str2 := encoding.EncodeToString(bytes)
	str3, err := encoding.DecodeString(str2)
	if err != nil {
		return err
	}

	var encStart, encEnd, decStart, decEnd time.Time
	sizeEncoded, sizeDecoded := 0, 0

	err = common.RunBenchmark(name, func() {
		encStart = time.Now()
		for i := 0; i < tries; i += 1 {
			sizeEncoded += len(encoding.EncodeToString(bytes))
		}
		encEnd = time.Now()

		decStart = time.Now()
		for i := 0; i < tries; i += 1 {
			decoded, err := encoding.DecodeString(str2)
			if err != nil {
				return
			}
			sizeDecoded += len(decoded)
		}
		decEnd = time.Now()
	})
	if err != nil {
		return err
	}
	fmt.Printf("encode %s... to %s...: %d, %.4f\n",
		string(bytes[:4]), str2[:4], sizeEncoded, encEnd.Sub(encStart).Seconds())
	fmt.Printf("decode %s... to %s...: %d, %.4f\n",
		str2[:4], string(str3[:4]), sizeDecoded, decEnd.Sub(decStart).Seconds())

	return nil
}
