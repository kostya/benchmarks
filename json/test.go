package main

import (
	"encoding/json"
	"fmt"
	"net"
	"os"
	"runtime"
)

type Coordinate struct {
	X, Y, Z float64
}

type TestStruct struct {
	Coordinates []Coordinate
}

func main() {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, runtime.Compiler)
		conn.Close()
	}

	f, err := os.Open("./1.json")
	if err != nil {
		panic(err)
	}

	jobj := TestStruct{}
	err = json.NewDecoder(f).Decode(&jobj)
	if err != nil {
		panic(err)
	}

	x, y, z := 0.0, 0.0, 0.0

	for _, coord := range jobj.Coordinates {
		x += coord.X
		y += coord.Y
		z += coord.Z
	}

	len := float64(len(jobj.Coordinates))
	fmt.Printf("%.8f\n%.8f\n%.8f\n", x/len, y/len, z/len)
}
