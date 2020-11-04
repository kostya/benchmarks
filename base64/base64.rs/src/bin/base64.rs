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
    for [src, dst] in &[["hello", "aGVsbG8="], ["world", "d29ybGQ="]] {
        let encoded = encode(&src);
        if encoded != *dst {
            eprintln!("{:?} != {:?}", encoded, dst);
            std::process::exit(-1);
        }
        let decoded = String::from_utf8(decode(&dst).unwrap()).unwrap();
        if decoded != *src {
            eprintln!("{:?} != {:?}", decoded, src);
            std::process::exit(-1);
        }
    }

    let input = vec![b'a'; STR_SIZE];
    let output = encode(&input);
    let str3 = decode(&output).unwrap();

    notify(&format!("Rust\t{}", std::process::id()));

    let time_start = Instant::now();
    let mut sum_encoded = 0;
    for _ in 0..TRIES {
        sum_encoded += encode(&input).len();
    }
    let t_encoded = time_start.elapsed().as_secs_f32();

    let t1_start = Instant::now();
    let mut sum_decoded = 0;
    for _ in 0..TRIES {
        sum_decoded += decode(&output).unwrap().len();
    }
    let t_decoded = t1_start.elapsed().as_secs_f32();

    notify("stop");

    println!(
        "encode {}... to {}...: {}, {}",
        str::from_utf8(&input[..4]).unwrap(),
        &output[..4],
        sum_encoded,
        t_encoded
    );

    println!(
        "decode {}... to {}...: {}, {}",
        &output[..4],
        str::from_utf8(&str3[..4]).unwrap(),
        sum_decoded,
        t_decoded
    );
}
