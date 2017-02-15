extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use serde::{Deserializer, de};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::marker::PhantomData;
use std::fmt;

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
struct State {
    x: f64,
    y: f64,
    z: f64,
    len: usize,
}

#[derive(Deserialize)]
pub struct TestStruct  {
    #[serde(deserialize_with = "deserialize_add", rename(deserialize = "coordinates"))]
    state: State,
    #[serde(skip_deserializing)]
    _info: (),
}

fn deserialize_add<D>(deserializer: D) -> Result<State, D::Error>
    where D: Deserializer
{
    struct StateVisitor<State>(PhantomData<State>);

    impl de::Visitor for StateVisitor<State>
    {
        type Value = State;
        
        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(formatter, "a state")
        }

        fn visit_seq<V>(self, mut visitor: V) -> Result<State, V::Error>
            where V: de::SeqVisitor
        {
            let mut ac = State {x: 0.0, y: 0.0, z: 0.0, len: 1};
            while let Some(v) = visitor.visit::<Coordinate>()? {
                ac.x += v.x;
                ac.y += v.y;
                ac.z += v.z;
                ac.len += 1;
            }

            Ok(ac)
        }
    }

    let visitor = StateVisitor(PhantomData);
    deserializer.deserialize_seq(visitor)
}

fn main() {
    let path = Path::new("./1.json");
    let mut s = String::new();
    let mut file = File::open(&path).unwrap();
    file.read_to_string(&mut s).unwrap();

    let test: TestStruct = serde_json::from_str(&s).unwrap();

    let len = test.state.len as f64;
    println!("{}", test.state.x / len);
    println!("{}", test.state.y / len);
    println!("{}", test.state.z / len);
}
