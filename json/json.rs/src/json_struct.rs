use serde::Deserialize;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::net::TcpStream;
use std::{fs, process};

#[derive(Deserialize, PartialEq)]
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

#[derive(Deserialize)]
struct TestStruct {
    coordinates: Vec<Coordinate>,
}

fn notify(msg: &str) {
    if let Ok(mut stream) = TcpStream::connect(("127.0.0.1", 9001)) {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(s: &str) -> Coordinate {
    let jobj = serde_json::from_str::<TestStruct>(s).unwrap();

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
        r#"{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}"#,
        r#"{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}"#,
    ] {
        let left = calc(v);
        if left != right {
            eprintln!("{left} != {right}");
            process::exit(-1);
        }
    }

    let s = fs::read_to_string("/tmp/1.json").unwrap_or_default();

    notify(&format!("Rust (Serde Typed)\t{pid}", pid = process::id()));
    let results = calc(&s);
    notify("stop");

    println!("{results}");
}
