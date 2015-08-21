extern crate serde;
extern crate serde_json;

use serde_json::Value;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() {
    let path = Path::new("./1.json");
    let mut s = Vec::new();
    let mut file = File::open(&path).unwrap();
    file.read_to_end(&mut s).unwrap();

    let value: Value = serde_json::de::from_slice(&s).unwrap();

    let coordinates = value.find("coordinates").unwrap().as_array().unwrap();

    let len = coordinates.len() as f64;
    let mut x = 0_f64;
    let mut y = 0_f64;
    let mut z = 0_f64;

    for coord in coordinates.iter() {
        x += coord.find("x").unwrap().as_f64().unwrap();
        y += coord.find("y").unwrap().as_f64().unwrap();
        z += coord.find("z").unwrap().as_f64().unwrap();
    }

    println!("{}", x / len);
    println!("{}", y / len);
    println!("{}", z / len);
}
