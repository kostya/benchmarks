#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate serde;
extern crate serde_json;

use serde::{Deserialize, Deserializer, de};
use std::fs::File;
use std::io::Read;
use std::path::Path;


#[derive(Deserialize)]
pub struct Coordinate {
  x: f64,
  y: f64,
  z: f64,
  #[allow(dead_code)]
  name: Skip,
  #[allow(dead_code)]
  opts: Skip,
}


#[derive(Deserialize)]
pub struct TestStruct  {
  coordinates: Vec<Coordinate>,
  #[allow(dead_code)]
  info: Skip,
}

fn main() {
  let path = Path::new("./1.json");
  let mut s = Vec::new();
  let mut file = File::open(&path).unwrap();
  file.read_to_end(&mut s).unwrap();

  let jobj: TestStruct = serde_json::de::from_slice(&s).unwrap();

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

// A simple type which deserializes by silently consuming all its input, allowing a field to be
// skipped.
struct Skip;
impl Deserialize for Skip {
  fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error> where D: Deserializer {
      deserializer.visit(Skip)
  }
}

impl de::Visitor for Skip {
  type Value = Skip;

  fn visit_map<V>(&mut self, mut visitor: V) -> Result<Skip, V::Error> where V: de::MapVisitor {
    while try!(visitor.visit::<Skip, Skip>()).is_some() {}
    try!(visitor.end());
    Ok(Skip)
  }

  fn visit_seq<V>(&mut self, mut visitor: V) -> Result<Skip, V::Error> where V: de::SeqVisitor {
    while try!(visitor.visit::<Skip>()).is_some() {}
    try!(visitor.end());
    Ok(Skip)
  }

  fn visit_str<E>(&mut self, _v: &str) -> Result<Skip, E> where E: de::Error { Ok(Skip) }
  fn visit_u64<E>(&mut self, _v: u64) -> Result<Skip, E> where E: de::Error { Ok(Skip) }
  fn visit_bool<E>(&mut self, _v: bool) -> Result<Self::Value, E> where E: de::Error { Ok(Skip) }
}

