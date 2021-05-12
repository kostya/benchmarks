extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use std::fmt;
use std::fs;
use std::str;

#[derive(Deserialize, PartialEq)]
struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

impl fmt::Display for Coordinate {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Coordinate {{ x: {:e}, y: {:e}, z: {} }}",
            self.x, self.y, self.z
        )
    }
}

#[derive(Deserialize)]
struct TestStruct {
    coordinates: Vec<Coordinate>,
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(s: &str) -> Coordinate {
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

    Coordinate {
        x: x / len,
        y: y / len,
        z: z / len,
    }
}

fn main() {
    let right = Coordinate {
        x: 2.0,
        y: 0.5,
        z: 0.25,
    };
    for v in &[
        "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
        "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}",
    ] {
        let left = calc(v);
        if left != right {
            eprintln!("{} != {}", left, right);
            std::process::exit(-1);
        }
    }

    let s = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (Serde Typed)\t{}", std::process::id()));
    let results = calc(&s);
    notify("stop");

    println!("{}", results);
}
