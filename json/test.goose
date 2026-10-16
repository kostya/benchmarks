// Json: the mean of the coordinates in a large JSON document, read straight
// into the program's own types with the standard library's json::Reader, as
// serde's derived deserializers read them.

import std;
import os;
import json;

extern "goose_notify_start" fn notify_start(variant: const u8[:]);
extern "goose_notify_stop" fn notify_stop();

struct Coordinate { x: f64, y: f64, z: f64 }

fn format(out: u8[>..]&, c: Coordinate) {
    format(out, "Coordinate { x: ", c.x, ", y: ", c.y, ", z: ", c.z, " }");
}

// The members a test struct does not name are passed over, as serde passes
// over the fields a struct does not have.
fn read_coordinate(r: json::Reader&) -> Coordinate {
    var c = Coordinate { 0.0, 0.0, 0.0 };
    json::each_member(r) { key =>
        if key == "x" { c.x = json::read_number(r); }
        else if key == "y" { c.y = json::read_number(r); }
        else if key == "z" { c.z = json::read_number(r); }
        else { json::skip(r); }
    };
    return c;
}

fn read_coordinates(text: const u8[:]) -> Coordinate[>..] {
    var coordinates: Coordinate[>..] = [];
    var r = json::reader(text);
    json::each_member(r) { key =>
        if key == "coordinates" {
            json::each_element(r) { coordinates.push(read_coordinate(r)); };
        } else {
            json::skip(r);
        }
    };
    if !json::finish(r) { abort(json::message(r)); }
    return coordinates;
}

fn calc(text: const u8[:]) -> Coordinate {
    let coordinates = read_coordinates(text);
    let len = coordinates.len as f64;
    var x = 0.0;
    var y = 0.0;
    var z = 0.0;
    for c in coordinates {
        x += c.x;
        y += c.y;
        z += c.z;
    }
    return Coordinate { x / len, y / len, z / len };
}

fn main() {
    let right = Coordinate { 2.0, 0.5, 0.25 };
    for v in ["{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
              "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"] {
        let left = calc(v);
        if left != right { abort(str(left, " != ", right)); }
    }

    var text: u8[>..] = [];
    read_file("/tmp/1.json", text);

    notify_start(" (Typed)");
    let results = calc(text);
    notify_stop();

    print(results);
}
