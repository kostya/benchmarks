use std::fmt;
use std::fs;
use std::str;

#[derive(PartialEq)]
struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
}

impl fmt::Display for Coordinate {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Coordinate {{ x: {:e}, y: {:e}, z: {} }}",
            self.x, self.y, self.z
        )
    }
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn calc(program: &mut jq_rs::JqProgram, content: &str) -> Coordinate {
    let result = program.run(&content).unwrap();
    let mut iter = result.split_whitespace();
    Coordinate {
        x: iter.next().unwrap().parse().unwrap(),
        y: iter.next().unwrap().parse().unwrap(),
        z: iter.next().unwrap().parse().unwrap(),
    }
}

fn main() {
    let mut program = jq_rs::compile(".coordinates | length as $len | (map(.x) | add) / $len, (map(.y) | add) / $len, (map(.z) | add) / $len").unwrap();

    let right = Coordinate {
        x: 2.0,
        y: 0.5,
        z: 0.25,
    };
    for v in &[
        "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
        "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}",
    ] {
        let left = calc(&mut program, v);
        if left != right {
            eprintln!("{} != {}", left, right);
            std::process::exit(-1);
        }
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (jq)\t{}", std::process::id()));
    let results = calc(&mut program, &content);
    notify("stop");

    println!("{}", results);
}
