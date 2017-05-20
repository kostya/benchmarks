package main

import (
	"encoding/base64"
	"fmt"
	"strings"
	"time"
)

const (
	strSize = 10000000
	tries   = 100
)

var (
	bytes      = []byte(strings.Repeat("a", strSize))
	coder      = base64.StdEncoding
	str2       = ""
	str3       = []byte{}
	encodeChan = make(chan int)
	decodeChan = make(chan int)
	ct         = 0
)

func main() {
	t := time.Now()
	s := 0

	for i := 0; i < tries; i++ {
		go func(encodeChan chan int) {
			str2 = coder.EncodeToString(bytes)
			encodeChan <- len(str2)
		}(encodeChan)
	}

	for {
		i := <-encodeChan
		ct++
		s += i
		if ct == tries {
			close(encodeChan)
			ct = 0
			break
		}
	}

	fmt.Printf("encode: %d, %.4f\n", s, time.Since(t).Seconds())

	t = time.Now()
	s = 0
	for i := 0; i < tries; i++ {
		go func(decodeChan chan int) {
			str3, _ = coder.DecodeString(str2)
			decodeChan <- len(str3)
		}(decodeChan)
	}

	for {
		i := <-decodeChan
		ct++
		s += i
		if ct == tries {
			close(decodeChan)
			break
		}
	}

	fmt.Printf("decode: %d, %.4f\n", s, time.Since(t).Seconds())
}
