package main

import "core:encoding/json"
import "core:fmt"
import "core:net"
import "core:os"
import "core:os/os2"

Coordinate :: struct {
	x, y, z: f64,
}

TestStruct :: struct {
	coordinates: []Coordinate,
}

notify :: proc(msg: string) {
	sock, _ := net.dial_tcp("127.0.0.1:9001")
	defer net.close(sock)
	net.send(sock, transmute([]byte)msg)
}

calc :: proc(s: string) -> Coordinate {
	j: TestStruct
	if err := json.unmarshal_string(s, &j); err != nil {
		panic(fmt.tprintf("%v", err))
	}

	x := 0.0
	y := 0.0
	z := 0.0
	len := 0.0
	for coord in j.coordinates {
		x += coord.x
		y += coord.y
		z += coord.z
		len += 1
	}
	return Coordinate{
		x = x / len,
		y = y / len,
		z = z / len,
	}
}

main :: proc() {
	context.allocator = context.temp_allocator
	defer free_all(context.temp_allocator)

	right := Coordinate{2.0, 0.5, 0.25}
	for v in ([]string {
			`{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
			`{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`,
		}) {
		left := calc(v)
		assert(left == right)
	}

	s, ok := os.read_entire_file_from_filename("/tmp/1.json")
	if !ok {
		panic("Failed to read 1.json")
	}

	notify(fmt.tprintf("Odin\t%d", os2.get_pid()))
	results := calc(string(s))
	notify("stop")

	fmt.println(results)
}
