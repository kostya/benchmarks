extern crate serialize;
use std::io::File;
use std::str;
use serialize::json;

#[deriving(Decodable, Encodable)]
pub struct Coordinate {
  x: f64,
  y: f64,
  z: f64
}

#[deriving(Decodable, Encodable)]
pub struct TestStruct  {
  coordinates: Vec<Coordinate>
}

fn main() {
  let bytes = File::open(&Path::new("./1.json".as_slice())).read_to_end().unwrap();
  let text = str::from_utf8(bytes.as_slice()).unwrap();
  let decoded: TestStruct = json::decode(text).unwrap();

  let mut x = 0_f64;
  let mut y = 0_f64;
  let mut z = 0_f64;

  for coordinate in decoded.coordinates.iter() {
    x += coordinate.x;
    y += coordinate.y;
    z += coordinate.z;
  }

  let len = decoded.coordinates.len() as f64;
  println!("{}", x / len);
  println!("{}", y / len);
  println!("{}", z / len);
}
