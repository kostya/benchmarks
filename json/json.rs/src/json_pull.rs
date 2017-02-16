extern crate memmap;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use memmap::{Mmap, Protection};
use serde::{Deserializer, de};
use std::fmt;
use std::str;

#[derive(Deserialize)]
pub struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

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
}

fn deserialize_add<D>(deserializer: D) -> Result<State, D::Error>
    where D: Deserializer
{
    struct StateVisitor;

    impl de::Visitor for StateVisitor {
        type Value = State;
        
        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(formatter, "an array of coordinates")
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

    deserializer.deserialize_seq(StateVisitor)
}

fn main() {
    let file = Mmap::open_path("1.json", Protection::Read).unwrap();
    // Unsafe because we must guarantee that the file is not concurrently modified.
    let bytes = unsafe { file.as_slice() };
    let s = str::from_utf8(bytes).unwrap();

    let test: TestStruct = serde_json::from_str(s).unwrap();

    let len = test.state.len as f64;
    println!("{}", test.state.x / len);
    println!("{}", test.state.y / len);
    println!("{}", test.state.z / len);
}
