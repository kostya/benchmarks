import json
import os

struct Coordinate {
	x f64
	y f64
	z f64
}

struct Coordinates {
	coordinates []Coordinate
}

fn main() {
	s := os.read_file("./1.json") or {
		eprintln('Failed to open file 1.json')
		return
	}

	j := json.decode(Coordinates, s) or {
		eprintln('Failed to parse json')
		return
	}

	mut x := f64(0.0)
	mut y := f64(0.0)
	mut z := f64(0.0)

	for coord in j.coordinates {
		x += coord.x
		y += coord.y
		z += coord.z
	}

	len := j.coordinates.len
	x /= len
	y /= len
	z /= len

	println('$x\n$y\n$z')
}
