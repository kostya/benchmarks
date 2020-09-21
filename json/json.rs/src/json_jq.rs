use std::fs;
use std::str;

#[derive(Debug, PartialEq)]
pub struct Coordinate {
    x: f64,
    y: f64,
    z: f64,
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

    let left = calc(
        &mut program,
        "{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}",
    );
    let right = Coordinate {
        x: 1.1,
        y: 2.2,
        z: 3.3,
    };
    if left != right {
        eprintln!("{:?} != {:?}", left, right);
        std::process::exit(-1);
    }

    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust (jq)\t{}", std::process::id()));

    println!("{:?}", calc(&mut program, &content));

    notify("stop");
}
