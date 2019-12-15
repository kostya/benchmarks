import json
import os
import net

struct Coordinate {
	x f64
	y f64
	z f64
}

struct Coordinates {
	coordinates []Coordinate
}

fn notify() {
    sock := net.dial('127.0.0.1', 9001) or {
        return
    }
    mut lang := "V GCC"
    $if clang {
      lang = "V Clang"
    }
    sock.write(lang) or {}
    sock.close() or {}
}

fn main() {
	notify()

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
