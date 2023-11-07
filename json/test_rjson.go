package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"os"

	"github.com/willabides/rjson"
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

	var buffer rjson.Buffer
	var currentCoord *Coordinate

	coordHandler := rjson.ObjectValueHandlerFunc(func(fieldname, data []byte) (int, error) {
		switch string(fieldname) {
		case "x":
			return rjson.DecodeFloat64(data, &currentCoord.X)
		case "y":
			return rjson.DecodeFloat64(data, &currentCoord.Y)
		case "z":
			return rjson.DecodeFloat64(data, &currentCoord.Z)
		}
		return rjson.SkipValueFast(data, &buffer)
	})

	coordsHandler := rjson.ArrayValueHandlerFunc(func(data []byte) (int, error) {
		jobj.Coordinates = append(jobj.Coordinates, Coordinate{})
		currentCoord = &jobj.Coordinates[len(jobj.Coordinates)-1]
		return rjson.HandleObjectValues(data, coordHandler, &buffer)
	})

	rjson.HandleObjectValues(bytes, rjson.ObjectValueHandlerFunc(func(fieldname, data []byte) (int, error) {
		if string(fieldname) != "coordinates" {
			return rjson.SkipValueFast(data, &buffer)
		}
		return rjson.HandleArrayValues(data, coordsHandler, &buffer)
	}), &buffer)

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
		left := calc([]byte(v))
		if left != right {
			log.Fatalf("%+v != %+v\n", left, right)
		}
	}

	bytes, err := ioutil.ReadFile("/tmp/1.json")
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	notify(fmt.Sprintf("Go (rjson)\t%d", os.Getpid()))
	results := calc(bytes)
	notify("stop")

	fmt.Printf("%+v\n", results)
}
