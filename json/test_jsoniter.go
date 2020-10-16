package main

import (
	"fmt"
	"github.com/json-iterator/go"
	"io/ioutil"
	"log"
	"net"
	"os"
	"strings"
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

func calc(reader *strings.Reader) Coordinate {
	// Add jsoniter
	var json = jsoniter.ConfigCompatibleWithStandardLibrary

	jobj := TestStruct{}
	err := json.NewDecoder(reader).Decode(&jobj)
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
	return Coordinate{x / len, y / len, z / len}
}

func main() {
	right := Coordinate{1.1, 2.2, 3.3}
	for _, v := range []string{
		`{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}`,
		`{"coordinates":[{"y":2.2,"x":1.1,"z":3.3}]}`} {
		left := calc(strings.NewReader(v))
		if left != right {
			log.Fatalf("%+v != %+v\n", left, right)
		}
	}

	bytes, err := ioutil.ReadFile("/tmp/1.json")
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}
	content := string(bytes)
	reader := strings.NewReader(content)

	notify(fmt.Sprintf("Go (jsoniter)\t%d", os.Getpid()))

	fmt.Printf("%+v\n", calc(reader))

	notify("stop")
}
