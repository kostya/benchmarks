extern crate rustc_serialize;
use rustc_serialize::json;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

#[derive(RustcDecodable, RustcEncodable)]
pub struct Coordinate {
  x: f64,
  y: f64,
  z: f64
}

#[derive(RustcDecodable, RustcEncodable)]
pub struct TestStruct  {
  coordinates: Vec<Coordinate>
}

fn main() {
  let path = Path::new("./1.json");
  let mut s = String::new();
  let mut file = File::open(&path).unwrap();
  file.read_to_string(&mut s).unwrap();

  let jobj: TestStruct = json::decode(&s).unwrap();

  let len = jobj.coordinates.len() as f64;
  let mut x = 0_f64;
  let mut y = 0_f64;
  let mut z = 0_f64;

  for coord in jobj.coordinates.iter() {
    x += coord.x;
    y += coord.y;
    z += coord.z;
  }

  println!("{}", x / len);
  println!("{}", y / len);
  println!("{}", z / len);
}
