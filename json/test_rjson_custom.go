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

func notify(msg string) {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, msg)
		conn.Close()
	}
}

type handler struct {
	buffer rjson.Buffer
	coord  Coordinate
	count  float64 // used to calculate the running average
}

func (h *handler) HandleArrayValue(data []byte) (p int, err error) {
	h.count++
	return rjson.HandleObjectValues(data, h, &h.buffer)
}

// HandleObjectValue handles both the top level object where we only care about the "coordinates" field
// and the object inside the array where we keep a running average of x, y, and z values.
func (h *handler) HandleObjectValue(fieldname, data []byte) (int, error) {
	var ptr *float64 // points to X, Y or Z depending on the fieldname
	switch string(fieldname) {
	case "x":
		ptr = &h.coord.X
	case "y":
		ptr = &h.coord.Y
	case "z":
		ptr = &h.coord.Z
	case "coordinates":
		return rjson.HandleArrayValues(data, h, &h.buffer)
	default:
		return rjson.SkipValueFast(data, &h.buffer)
	}
	val, p, err := rjson.ReadFloat64(data)
	if err != nil {
		return p, err
	}
	*ptr += val
	return p, nil
}

func calc(bytes []byte) Coordinate {
	h := &handler{}
	rjson.HandleObjectValues(bytes, h, &h.buffer)
	return Coordinate{
		X: h.coord.X / h.count,
		Y: h.coord.Y / h.count,
		Z: h.coord.Z / h.count,
	}
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

	notify(fmt.Sprintf("Go (rjson custom)\t%d", os.Getpid()))
	results := calc(bytes)
	notify("stop")

	fmt.Printf("%+v\n", results)
}
