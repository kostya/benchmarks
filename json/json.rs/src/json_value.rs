extern crate memmap;
extern crate serde;
extern crate serde_json;

use memmap::{Mmap, Protection};
use serde_json::Value;
use std::str;

fn main() {
    let file = Mmap::open_path("1.json", Protection::Read).unwrap();
    // Unsafe because we must guarantee that the file is not concurrently modified.
    let bytes = unsafe { file.as_slice() };
    let s = str::from_utf8(bytes).unwrap();

    let value: Value = serde_json::from_str(s).unwrap();

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
