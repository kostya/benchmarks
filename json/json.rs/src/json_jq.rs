use jq_rs::{self, JqProgram};
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::net::TcpStream;
use std::{fs, process, str};

#[derive(PartialEq)]
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

fn notify(msg: &str) {
    if let Ok(mut stream) = TcpStream::connect(("127.0.0.1", 9001)) {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(program: &mut JqProgram, content: &str) -> Coordinate {
    let result = program.run(content).unwrap();
    let mut iter = result.split_whitespace();
    Coordinate {
        x: iter.next().unwrap().parse().unwrap(),
        y: iter.next().unwrap().parse().unwrap(),
        z: iter.next().unwrap().parse().unwrap(),
    }
}

fn main() {
    let mut program = jq_rs::compile(concat!(
        ".coordinates | length as $len |",
        " (map(.x) | add) / $len,",
        " (map(.y) | add) / $len,",
        " (map(.z) | add) / $len"
    ))
    .unwrap();

    let right = Coordinate {
        x: 2.0,
        y: 0.5,
        z: 0.25,
    };
    for v in &[
        r#"{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}"#,
        r#"{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}"#,
    ] {
        let left = calc(&mut program, v);
        if left != right {
            eprintln!("{left} != {right}");
            process::exit(-1);
        }
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (jq)\t{pid}", pid = process::id()));
    let results = calc(&mut program, &content);
    notify("stop");

    println!("{results}");
}
