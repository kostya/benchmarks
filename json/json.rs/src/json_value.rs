use serde_json::Value;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::net::TcpStream;
use std::{fs, process, str};

#[derive(PartialEq)]
struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

impl Display for Coordinate {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Coordinate {{ x: {:e}, y: {:e}, z: {} }}",
            self.x, self.y, self.z
        )
    }
}

fn notify(msg: &str) {
    if let Ok(mut stream) = TcpStream::connect(("127.0.0.1", 9001)) {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(content: &str) -> Coordinate {
    let value = serde_json::from_str::<Value>(content).unwrap();

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
    let right = Coordinate {
        x: 2.0,
        y: 0.5,
        z: 0.25,
    };
    for v in &[
        r#"{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}"#,
        r#"{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}"#,
    ] {
        let left = calc(v);
        if left != right {
            eprintln!("{left} != {right}");
            process::exit(-1);
        }
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (Serde Untyped)\t{pid}", pid = process::id()));
    let results = calc(&content);
    notify("stop");

    println!("{results}");
}
