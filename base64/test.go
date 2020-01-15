package main

import (
	"encoding/base64"
	"fmt"
	"net"
	"os"
	"runtime"
	"strings"
	"time"
)

func notify(msg string) {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, msg)
		conn.Close()
	}
}

func main() {
	STR_SIZE := 131072
	TRIES := 8192

	bytes := []byte(strings.Repeat("a", STR_SIZE))
	coder := base64.StdEncoding

	notify(fmt.Sprintf("%s\t%d", runtime.Compiler, os.Getpid()))
	t := time.Now()
	s := 0

	str2 := coder.EncodeToString(bytes)
	fmt.Printf("encode %s... to %s...: ", string(bytes[:4]), str2[:4])

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

	notify("stop")
}
