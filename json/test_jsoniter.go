package main

import (
	"fmt"
	jsoniter "github.com/json-iterator/go"
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
	right := Coordinate{2.0, 0.5, 0.25}
	for _, v := range []string{
		`{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
		`{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`} {
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
	results := calc(reader)
	notify("stop")

	fmt.Printf("%+v\n", results)
}
