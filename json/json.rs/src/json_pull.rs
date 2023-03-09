use serde::de::{SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::{fs, process};
use utils::notify;

#[derive(Deserialize, PartialEq)]
struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

impl Display for Coordinate {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Coordinate {{ x: {:e}, y: {:e}, z: {} }}",
            self.x, self.y, self.z
        )
    }
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

    impl<'de> Visitor<'de> for StateVisitor {
        type Value = State;

        fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
            write!(formatter, "an array of coordinates")
        }

        fn visit_seq<V>(self, mut visitor: V) -> Result<State, V::Error>
        where
            V: SeqAccess<'de>,
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

fn calc(content: &str) -> Coordinate {
    let test = serde_json::from_str::<TestStruct>(content).unwrap();
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
        x: 2.0,
        y: 0.5,
        z: 0.25,
    };
    for v in &[
        r#"{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}"#,
        r#"{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}"#,
    ] {
        let left = calc(v);
        if left != right {
            eprintln!("{left} != {right}");
            process::exit(-1);
        }
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap_or_default();

    notify!("Rust (Serde Custom)\t{pid}", pid = process::id());
    let results = calc(&content);
    notify!("stop");

    println!("{results}");
}
