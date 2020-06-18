import json
import os
import net

struct Coordinate {
	x f64
	y f64
	z f64
}

pub fn (c1 Coordinate) equals(c2 Coordinate) bool {
	return c1.x == c2.x && c1.y == c2.y && c1.z == c2.z
}

struct Coordinates {
	coordinates []Coordinate
}

fn notify(msg string) {
	sock := net.dial('127.0.0.1', 9001) or {
		return
	}
	sock.write(msg) or {
	}
	sock.close() or {
	}
}

fn calc(s string) Coordinate {
	j := json.decode(Coordinates, s) or {
		panic('Failed to parse json')
	}
	mut x := 0.0
	mut y := 0.0
	mut z := 0.0
	for coord in j.coordinates {
		x += coord.x
		y += coord.y
		z += coord.z
	}
	len := j.coordinates.len
	return Coordinate{
		x: x / len
		y: y / len
		z: z / len
	}
}

fn main() {
	left := calc('{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}')
	right := Coordinate{1.1, 2.2, 3.3}
	if !left.equals(right) {
		panic('$left != $right')
	}
	s := os.read_file('/tmp/1.json') or {
		panic('Failed to open file 1.json')
	}
	mut lang := 'V GCC'
	$if clang {
		lang = 'V Clang'
	}
	notify('${lang}\t${C.getpid()}')
	println(calc(s))
	notify('stop')
}
