extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use serde::{de, Deserializer};
use std::fmt;
use std::fs;
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
pub struct TestStruct {
    #[serde(
        deserialize_with = "deserialize_add",
        rename(deserialize = "coordinates")
    )]
    state: State,
}

fn deserialize_add<'de, D>(deserializer: D) -> Result<State, D::Error>
where
    D: Deserializer<'de>,
{
    struct StateVisitor;

    impl<'de> de::Visitor<'de> for StateVisitor {
        type Value = State;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(formatter, "an array of coordinates")
        }

        fn visit_seq<V>(self, mut visitor: V) -> Result<State, V::Error>
        where
            V: de::SeqAccess<'de>,
        {
            let mut ac = State {
                x: 0.0,
                y: 0.0,
                z: 0.0,
                len: 1,
            };
            while let Some(v) = visitor.next_element::<Coordinate>()? {
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

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn main() {
    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust Serde Custom\t{}", std::process::id()));

    let test: TestStruct = serde_json::from_str(&content).unwrap();

    let len = test.state.len as f64;
    println!("{}", test.state.x / len);
    println!("{}", test.state.y / len);
    println!("{}", test.state.z / len);

    notify("stop");
}
