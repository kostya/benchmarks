use jq_rs;
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
    let mut program = jq_rs::compile(".coordinates | length as $len | (map(.x) | add) / $len, (map(.y) | add) / $len, (map(.z) | add) / $len").unwrap();

    notify(&format!("Rust jq\t{}", std::process::id()));

    let result = program.run(&content).unwrap();
    println!("{}", result);

    notify("stop");
}
