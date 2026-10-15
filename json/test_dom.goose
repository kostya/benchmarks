// Json: the mean of the coordinates in a large JSON document, from the
// standard library's document tree, as serde_json's Value and RapidJSON's
// Document hand them over.

import std;
import os;
import json;

extern "goose_notify_start" fn notify_start(variant: const u8[:]);
extern "goose_notify_stop" fn notify_stop();

struct Coordinate { x: f64, y: f64, z: f64 }

fn format(out: u8[>..]&, c: Coordinate) {
    format(out, "Coordinate { x: ", c.x, ", y: ", c.y, ", z: ", c.z, " }");
}

fn number(n: json::Node?) -> f64 {
    let v, ok = json::as_f64(n);
    if !ok { abort("not a number"); }
    return v;
}

fn calc(text: const u8[:]) -> Coordinate {
    var doc: json::Node[>..] = [];
    var decoded: u8[>..] = [];
    let root, err = json::parse(text, doc, decoded);
    guard root else { abort(err); }
    let coordinates = json::member(root, "coordinates");
    guard coordinates else { abort("no coordinates"); }

    var len = 0;
    var x = 0.0;
    var y = 0.0;
    var z = 0.0;
    json::each(coordinates) { coord =>
        x += number(json::member(coord, "x"));
        y += number(json::member(coord, "y"));
        z += number(json::member(coord, "z"));
        len++;
    };
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

    notify_start(" (DOM)");
    let results = calc(text);
    notify_stop();

    print(results);
}
