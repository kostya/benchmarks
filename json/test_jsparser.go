package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/tamerh/jsparser"
	"io/ioutil"
	"net"
	"os"
	"strconv"
)

func notify(msg string) {
	conn, err := net.Dial("tcp", "localhost:9001")
	if err == nil {
		fmt.Fprintf(conn, msg)
		conn.Close()
	}
}

func main() {
	content, err := ioutil.ReadFile("/tmp/1.json")
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	notify(fmt.Sprintf("Go jsparser\t%d", os.Getpid()))

	f := bytes.NewReader(content)
	br := bufio.NewReaderSize(f, 16384)
	parser := jsparser.NewJSONParser(br, "coordinates").SkipProps([]string{"name", "opts"})
	x, y, z := 0.0, 0.0, 0.0
	len := 0.0

	for json := range parser.Stream() {
		xx, _ := strconv.ParseFloat(json.ObjectVals["x"].StringVal, 64)
		yy, _ := strconv.ParseFloat(json.ObjectVals["y"].StringVal, 64)
		zz, _ := strconv.ParseFloat(json.ObjectVals["z"].StringVal, 64)
		x += xx
		y += yy
		z += zz
		len += 1.0
	}
	fmt.Printf("%.8f\n%.8f\n%.8f\n", x/len, y/len, z/len)

	notify("stop")
}
