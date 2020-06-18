package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
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

func notify(msg string) {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, msg)
		conn.Close()
	}
}

func calc(bytes []byte) Coordinate {
	jobj := TestStruct{}
	json.Unmarshal(bytes, &jobj)

	x, y, z := 0.0, 0.0, 0.0

	for _, coord := range jobj.Coordinates {
		x += coord.X
		y += coord.Y
		z += coord.Z
	}

	len := float64(len(jobj.Coordinates))
	return Coordinate{x / len, y / len, z / len}
}

func main() {
	left := calc([]byte(`{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}`))
	right := Coordinate{1.1, 2.2, 3.3}
	if left != right {
		log.Fatalf("%+v != %+v\n", left, right)
	}

	bytes, err := ioutil.ReadFile("/tmp/1.json")
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	notify(fmt.Sprintf("%s\t%d", runtime.Compiler, os.Getpid()))

	fmt.Printf("%+v\n", calc(bytes))

	notify("stop")
}
