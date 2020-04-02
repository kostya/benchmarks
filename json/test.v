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

fn notify(msg string) {
	sock := net.dial('127.0.0.1', 9001) or {
		return
	}
	sock.write(msg) or { }
	sock.close() or { }
}

fn main() {
	s := os.read_file('/tmp/1.json') or {
		eprintln('Failed to open file 1.json')
		return
	}

	mut lang := 'V GCC'
	$if clang {
		lang = 'V Clang'
	}
	notify('${lang}\t${C.getpid()}')

	j := json.decode(Coordinates,s) or {
		eprintln('Failed to parse json')
		return
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
	x /= len
	y /= len
	z /= len
	println('$x\n$y\n$z')
	notify('stop')
}
