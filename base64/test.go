package main

import (
	"encoding/base64"
	"fmt"
	"log"
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
	coder := base64.StdEncoding

	for _, fixture := range [][]string{
		{"hello", "aGVsbG8="},
		{"world", "d29ybGQ="},
	} {
		src := fixture[0]
		dst := fixture[1]
		encoded := coder.EncodeToString([]byte(src))
		if encoded != dst {
			log.Fatalf("%+v != %+v\n", encoded, dst)
		}
		decoded, _ := coder.DecodeString(dst)
		if string(decoded) != src {
			log.Fatalf("%+v != %+v\n", decoded, src)
		}
	}

	STR_SIZE := 131072
	TRIES := 8192

	bytes := []byte(strings.Repeat("a", STR_SIZE))
	str2 := coder.EncodeToString(bytes)
	str3, _ := coder.DecodeString(str2)

	notify(fmt.Sprintf("%s\t%d", runtime.Compiler, os.Getpid()))

	t := time.Now()
	s_encoded := 0
	for i := 0; i < TRIES; i += 1 {
		s_encoded += len(coder.EncodeToString(bytes))
	}
	t_encoded := time.Since(t).Seconds()

	t1 := time.Now()
	s_decoded := 0
	for i := 0; i < TRIES; i += 1 {
		decoded, _ := coder.DecodeString(str2)
		s_decoded += len(decoded)
	}
	t_decoded := time.Since(t1).Seconds()

	notify("stop")

	fmt.Printf("encode %s... to %s...: %d, %.4f\n",
		string(bytes[:4]), str2[:4], s_encoded, t_encoded)
	fmt.Printf("decode %s... to %s...: %d, %.4f\n",
		str2[:4], string(str3[:4]), s_decoded, t_decoded)
}
