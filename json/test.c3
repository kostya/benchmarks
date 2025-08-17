module json_c3;

import std::io;
import std::os::posix;
import std::encoding::json;
import std::collections::object;
import std::collections::list;
import std::net::tcp;

struct Coordinate
{
	double x, y, z;
}

struct TestStruct
{
	Coordinate[] coordinates;
}

fn bool Coordinate.equals(&self, Coordinate other)
{
	return self.x == other.x && self.y == other.y && self.z == other.z;
}

fn usz? Coordinate.to_format(&self, Formatter *formatter) @dynamic
{
	return formatter.printf(`Coordinate{x: %.17e, y: %.17e, z: %.17e}`, self.x, self.y, self.z);
}

fn void notify(String msg)
{
	TcpSocket? sock = tcp::connect("127.0.0.1", 9001);
	if (catch excuse = sock)
	{
		return;
	}
	defer(void) sock.close();
	sock.write(msg)!!;
}

fn Coordinate? calc(String s)
{
	Object *o = json::parse_string(mem, s)!!;
	defer o.free();

	List{Object *} coords = o.get("coordinates")!!.array;

	double x, y, z;
	foreach(c : coords)
	{
		x += c.get("x")!!.f;
		y += c.get("y")!!.f;
		z += c.get("z")!!.f;
	}

	return (Coordinate) {
		x / coords.size,
		y / coords.size,
		z / coords.size,
	};
}

fn void main(String[] args)
{
	Coordinate right = {2.0, 0.5, 0.25};
	String[*] vals = {
		`{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
		`{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`,
	};
	foreach(v : vals)
	{
		Coordinate left = calc(v)!!;
		assert(left.equals(right));
	}

	char[] json_str = file::load_temp("/tmp/1.json")!!;

	notify(string::tformat("C3\t%d", posix::getpid()));
	Coordinate results = calc((String)json_str)!!;
	notify("end");

	io::printn(results);
}
