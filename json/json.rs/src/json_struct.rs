extern crate memmap;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use memmap::{Mmap, Protection};
use std::str;

#[derive(Deserialize)]
pub struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

#[derive(Deserialize)]
pub struct TestStruct  {
    coordinates: Vec<Coordinate>,
}

fn main() {
    let file = Mmap::open_path("1.json", Protection::Read).unwrap();
    // Unsafe because we must guarantee that the file is not concurrently modified.
    let bytes = unsafe { file.as_slice() };
    let s = str::from_utf8(bytes).unwrap();

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
