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
	mut sock := net.dial_tcp('127.0.0.1:9001') or { return }
	defer {
		sock.close() or {}
	}
	sock.write_string(msg) or {}
}

fn calc(s string) Coordinate {
	j := json.decode(Coordinates, s) or { panic('Failed to parse json') }
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
	right := Coordinate{2.0, 0.5, 0.25}
	for v in ['{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}',
		'{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}',
	] {
		left := calc(v)
		if !left.equals(right) {
			panic('$left != $right')
		}
	}
	s := os.read_file('/tmp/1.json') or { panic('Failed to open file 1.json') }
	mut lang := 'V/gcc'
	$if clang {
		lang = 'V/clang'
	}
	notify('$lang\t$C.getpid()')
	results := calc(s)
	notify('stop')
	println(results)
}
