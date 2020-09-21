use serde_json::Value;
use std::fs;
use std::str;

#[derive(Debug, PartialEq)]
pub struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(content: &str) -> Coordinate {
    let value: Value = serde_json::from_str(&content).unwrap();

    let coordinates = value.get("coordinates").unwrap().as_array().unwrap();

    let len = coordinates.len() as f64;
    let mut x = 0_f64;
    let mut y = 0_f64;
    let mut z = 0_f64;

    for coord in coordinates.iter() {
        x += coord.get("x").unwrap().as_f64().unwrap();
        y += coord.get("y").unwrap().as_f64().unwrap();
        z += coord.get("z").unwrap().as_f64().unwrap();
    }

    Coordinate {
        x: x / len,
        y: y / len,
        z: z / len,
    }
}

fn main() {
    let left = calc("{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}");
    let right = Coordinate {
        x: 1.1,
        y: 2.2,
        z: 3.3,
    };
    if left != right {
        eprintln!("{:?} != {:?}", left, right);
        std::process::exit(-1);
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (Serde Untyped)\t{}", std::process::id()));

    println!("{:?}", calc(&content));

    notify("stop");
}
