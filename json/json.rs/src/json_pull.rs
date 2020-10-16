extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use serde::{de, Deserializer};
use std::fmt;
use std::fs;
use std::str;

#[derive(Deserialize, Debug, PartialEq)]
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
                len: 0,
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

fn calc(content: &str) -> Coordinate {
    let test: TestStruct = serde_json::from_str(&content).unwrap();
    let state = test.state;
    let len = state.len as f64;
    Coordinate {
        x: state.x / len,
        y: state.y / len,
        z: state.z / len,
    }
}

fn main() {
    let right = Coordinate {
        x: 1.1,
        y: 2.2,
        z: 3.3,
    };
    for v in &[
        "{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}",
        "{\"coordinates\":[{\"y\":2.2,\"x\":1.1,\"z\":3.3}]}",
    ] {
        let left = calc(v);
        if left != right {
            eprintln!("{:?} != {:?}", left, right);
            std::process::exit(-1);
        }
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (Serde Custom)\t{}", std::process::id()));

    println!("{:?}", calc(&content));

    notify("stop");
}
