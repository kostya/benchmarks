#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate serde;
extern crate serde_json;
extern crate json_rs;

use json_rs::Skip;
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

fn main() {
  let path = Path::new("./1.json");
  let mut s = Vec::new();
  let mut file = File::open(&path).unwrap();
  file.read_to_end(&mut s).unwrap();

  let state: State = serde_json::de::from_slice(&s).unwrap();

  let len = state.len as f64;
  println!("{}", state.x / len);
  println!("{}", state.y / len);
  println!("{}", state.z / len);
}

struct State {
    x: f64,
    y: f64,
    z: f64,
    len: usize,
}

impl Deserialize for State {
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: Deserializer
    {
        deserializer.visit(StateVisitor)
    }
}

struct StateVisitor;

impl de::Visitor for StateVisitor {
    type Value = State;

    fn visit_map<V>(&mut self,
                    mut visitor: V) -> Result<State, V::Error>
        where V: de::MapVisitor
    {
        let mut state = None;

        while let Some(key) = try!(visitor.visit_key()) {
            match key {
                TestStructField::Coordinates => {
                    state = Some(try!(visitor.visit_value()));
                }
                TestStructField::Info => {
                    let _: Skip = try!(visitor.visit_value());
                }
            }
        }

        try!(visitor.end());

        Ok(state.unwrap())
    }

    fn visit_seq<V>(&mut self,
                    mut visitor: V) -> Result<State, V::Error>
        where V: de::SeqVisitor
    {
        let mut state = State {
            x: 0.0,
            y: 0.0,
            z: 0.0,
            len: 0,
        };

        while let Some(coordinate) = try!(visitor.visit::<Coordinate>()) {
            state.x += coordinate.x;
            state.y += coordinate.y;
            state.z += coordinate.z;
            state.len += 1;
        }

        try!(visitor.end());

        Ok(state)
    }
}

enum TestStructField {
    Coordinates,
    Info,
}

impl Deserialize for TestStructField {
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: Deserializer
    {
        deserializer.visit(TestStructFieldVisitor)
    }
}

struct TestStructFieldVisitor;

impl de::Visitor for TestStructFieldVisitor {
    type Value = TestStructField;

    fn visit_str<E>(&mut self, v: &str) -> Result<Self::Value, E>
        where E: de::Error,
    {
        match v {
            "coordinates" => Ok(TestStructField::Coordinates),
            "info" => Ok(TestStructField::Info),
            _ => Err(E::unknown_field(v)),
        }
    }
}
