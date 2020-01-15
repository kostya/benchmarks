use serde_json::Value;
use std::fs;
use std::str;

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn main() {
    let content = fs::read_to_string("/tmp/1.json").unwrap();

    notify(&format!("Rust Serde Untyped\t{}", std::process::id()));

    let value: Value = serde_json::from_str(&content).unwrap();

    let coordinates = value.get("coordinates").unwrap().as_array().unwrap();

    let len = coordinates.len() as f64;
    let mut x = 0_f64;
    let mut y = 0_f64;
    let mut z = 0_f64;

    for coord in coordinates.iter() {
        x += coord.get("x").unwrap().as_f64().unwrap();
        y += coord.get("y").unwrap().as_f64().unwrap();
        z += coord.get("z").unwrap().as_f64().unwrap();
    }

    println!("{}", x / len);
    println!("{}", y / len);
    println!("{}", z / len);

    notify("stop");
}
