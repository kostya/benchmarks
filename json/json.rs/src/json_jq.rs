use jq_rs;
use memmap::Mmap;
use std::fs::File;
use std::str;

fn main() {
    {
        use std::io::Write;
        if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
            stream.write_all(b"Rust jq").unwrap();
        }
    }

    let mut program = jq_rs::compile(".coordinates | length as $len | (map(.x) | add) / $len, (map(.y) | add) / $len, (map(.z) | add) / $len").unwrap();

    let file = File::open("1.json").unwrap();
    let mmap = unsafe { Mmap::map(&file).unwrap() };
    let content = str::from_utf8(&mmap[..]).unwrap();

    let result = program.run(&content).unwrap();
    println!("{}", result);
}
