extern crate memmap;
extern crate serde;
extern crate serde_json;

use memmap::Mmap;
use serde_json::Value;
use std::fs::File;
use std::str;

fn main() {
    {
        use std::io::Write;
        if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
            stream.write_all(b"Rust Serde Untyped").unwrap();
        }
    }

    let file = File::open("1.json").unwrap();
    let mmap = unsafe { Mmap::map(&file).unwrap() };
    let contents = str::from_utf8(&mmap[..]).unwrap();

    let value: Value = serde_json::from_str(&contents).unwrap();

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

    println!("{}", x / len);
    println!("{}", y / len);
    println!("{}", z / len);
}
