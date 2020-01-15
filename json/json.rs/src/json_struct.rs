extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use std::fs;
use std::str;

#[derive(Deserialize)]
pub struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

#[derive(Deserialize)]
pub struct TestStruct {
    coordinates: Vec<Coordinate>,
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn main() {
    let s = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust Serde Typed\t{}", std::process::id()));

    let jobj: TestStruct = serde_json::from_str(&s).unwrap();

    let len = jobj.coordinates.len() as f64;
    let mut x = 0_f64;
    let mut y = 0_f64;
    let mut z = 0_f64;

    for coord in &jobj.coordinates {
        x += coord.x;
        y += coord.y;
        z += coord.z;
    }

    println!("{}", x / len);
    println!("{}", y / len);
    println!("{}", z / len);

    notify("stop");
}
