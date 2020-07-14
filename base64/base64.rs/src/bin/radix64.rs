use radix64::FAST as base64;
use std::{io::Write, str};
use stopwatch::Stopwatch;

const STR_SIZE: usize = 131_072;
const TRIES: usize = 8192;

fn notify(msg: &[u8]) {
    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg).unwrap();
    }
}

fn main() {
    let input = vec![b'a'; STR_SIZE];
    let mut buffer = Vec::with_capacity(STR_SIZE);

    notify(&format!("Rust\t{}", std::process::id()).as_bytes());
    let mut sum = 0;

    let mut output = base64.encode_with_buffer(&input, &mut buffer);
    print!(
        "encode {}... to {}...: ",
        str::from_utf8(&input[..4]).unwrap(),
        &output[..4]
    );

    let mut sw = Stopwatch::start_new();
    for _ in 0..TRIES {
        output = base64.encode_with_buffer(&input, &mut buffer);
        sum += output.len();
    }
    let mut tim = sw.elapsed_ms();

    println!("{}, {}", sum, tim);

    let mut buffer = Vec::with_capacity(STR_SIZE);
    let mut str3 = base64.decode_with_buffer(&output, &mut buffer).unwrap();
    print!(
        "decode {}... to {}...: ",
        &output[..4],
        str::from_utf8(&str3[..4]).unwrap()
    );
    sum = 0;
    sw.restart();
    for _ in 0..TRIES {
        str3 = base64.decode_with_buffer(&output, &mut buffer).unwrap();
        sum += str3.len();
    }
    tim = sw.elapsed_ms();
    println!("{}, {}", sum, tim);

    notify(b"stop");
}
