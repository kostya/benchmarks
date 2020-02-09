package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"runtime"
	"bufio"
)

type Coordinate struct {
	X, Y, Z float64
}

type TestStruct struct {
	Coordinates []Coordinate
}

func notify(msg string) {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, msg)
		conn.Close()
	}
}

func main() {
        f, err := os.Open("/tmp/1.json")
        file := bufio.NewReaderSize(f, 16384)
        b, err := ioutil.ReadAll(file)
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	notify(fmt.Sprintf("%s\t%d", runtime.Compiler, os.Getpid()))

	jobj := TestStruct{}
        json.Unmarshal(b, &jobj)

	x, y, z := 0.0, 0.0, 0.0

	for _, coord := range jobj.Coordinates {
		x += coord.X
		y += coord.Y
		z += coord.Z
	}

	len := float64(len(jobj.Coordinates))
	fmt.Printf("%.8f\n%.8f\n%.8f\n", x/len, y/len, z/len)

	notify("stop")
}
