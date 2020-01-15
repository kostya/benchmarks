use base64::{decode, encode};
use std::str;
use std::time::Instant;

const STR_SIZE: usize = 131_072;
const TRIES: usize = 8192;

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn main() {
    let input = vec![b'a'; STR_SIZE];

    notify(&format!("Rust\t{}", std::process::id()));
    let mut time_start = Instant::now();
    let mut sum = 0;

    let mut output = encode(&input);
    print!(
        "encode {}... to {}...: ",
        str::from_utf8(&input[..4]).unwrap(),
        &output[..4]
    );

    for _ in 0..TRIES {
        output = encode(&input);
        sum += output.len();
    }
    println!("{}, {}", sum, time_start.elapsed().as_secs_f32());

    let mut str3 = decode(&output).unwrap();
    print!(
        "decode {}... to {}...: ",
        &output[..4],
        str::from_utf8(&str3[..4]).unwrap()
    );
    sum = 0;
    time_start = Instant::now();
    for _ in 0..TRIES {
        str3 = decode(&output).unwrap();
        sum += str3.len();
    }
    println!("{}, {}", sum, time_start.elapsed().as_secs_f32());

    notify("stop");
}
