extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Deserialize)]
pub struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
    #[serde(skip_deserializing)]
    _name: (),
    #[serde(skip_deserializing)]
    _opts: (),
}

#[derive(Deserialize)]
pub struct TestStruct  {
    coordinates: Vec<Coordinate>,
    #[serde(skip_deserializing)]
    _info: (),
}

fn main() {
    let path = Path::new("./1.json");
    let mut s = String::new();
    let mut file = File::open(&path).unwrap();
    file.read_to_string(&mut s).unwrap();

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
}
