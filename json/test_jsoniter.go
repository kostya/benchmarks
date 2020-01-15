package main

import (
	"fmt"
	"github.com/json-iterator/go"
	"io/ioutil"
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

func main() {
	bytes, err := ioutil.ReadFile("/tmp/1.json")
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}
	content := string(bytes)
	reader := strings.NewReader(content)

	notify(fmt.Sprintf("Go jsoniter\t%d", os.Getpid()))

	// Add jsoniter
	var json = jsoniter.ConfigCompatibleWithStandardLibrary

	jobj := TestStruct{}
	err = json.NewDecoder(reader).Decode(&jobj)
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

	notify("stop")
}
