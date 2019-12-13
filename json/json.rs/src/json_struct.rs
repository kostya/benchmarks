extern crate memmap;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use memmap::Mmap;

use std::fs::File;
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

fn main() {
    {
        use std::io::Write;
        if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
            stream.write_all(b"Rust Serde typed").unwrap();
        }
    }
    std::net::TcpStream::connect("localhost:9001").unwrap();

    let file = File::open("1.json").unwrap();
    let mmap = unsafe { Mmap::map(&file).unwrap() };
    let s = str::from_utf8(&mmap[..]).unwrap();

    let jobj: TestStruct = serde_json::from_str(s).unwrap();

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
}
