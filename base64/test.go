package main

import (
	"encoding/base64"
	"fmt"
	"net"
	"runtime"
	"strings"
	"time"
)

func main() {
	STR_SIZE := 131072
	TRIES := 8192

	bytes := []byte(strings.Repeat("a", STR_SIZE))

	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, runtime.Compiler)
		conn.Close()
	}

	coder := base64.StdEncoding

	str2 := coder.EncodeToString(bytes)
	fmt.Printf("encode %s... to %s...: ", string(bytes[:4]), str2[:4])

	t := time.Now()
	s := 0

	for i := 0; i < TRIES; i += 1 {
		str2 = coder.EncodeToString(bytes)
		s += len(str2)
	}
	fmt.Printf("%d, %.4f\n", s, time.Since(t).Seconds())

	t = time.Now()
	s = 0
	str3, _ := coder.DecodeString(str2)
	fmt.Printf("decode %s... to %s...: ", str2[:4], string(str3[:4]))

	for i := 0; i < TRIES; i += 1 {
		str3, _ = coder.DecodeString(str2)
		s += len(str3)
	}
	fmt.Printf("%d, %.4f\n", s, time.Since(t).Seconds())
}
