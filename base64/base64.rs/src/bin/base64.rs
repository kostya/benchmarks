use base64::Engine;
use std::time::Instant;
use std::{process, str};
use utils::notify;

const STR_SIZE: usize = 131_072;
const TRIES: usize = 8192;

fn main() {
    let base64_engine = base64::engine::general_purpose::STANDARD;

    for (src, dst) in [(b"hello", b"aGVsbG8="), (b"world", b"d29ybGQ=")] {
        let mut encoded = [0u8; 8];
        base64_engine.encode_slice(src, &mut encoded).unwrap();
        assert_eq!(encoded, *dst);

        let mut decoded = [0u8; 5];
        base64_engine
            .decode_slice_unchecked(dst, &mut decoded)
            .unwrap();
        assert_eq!(decoded, *src);
    }

    let input = [b'a'; STR_SIZE];

    let mut output = String::with_capacity(STR_SIZE * 4);
    base64_engine.encode_string(&input, &mut output);

    let mut str3 = [0u8; STR_SIZE];
    base64_engine
        .decode_slice_unchecked(&output, &mut str3)
        .unwrap();

    notify!("Rust\t{pid}", pid = process::id());

    let time_start = Instant::now();
    let mut sum_encoded = 0;
    for _ in 0..TRIES {
        sum_encoded += base64_engine.encode(&input).len();
    }
    let t_encoded = time_start.elapsed().as_secs_f32();

    let t1_start = Instant::now();
    let mut sum_decoded = 0;
    for _ in 0..TRIES {
        sum_decoded += base64_engine.decode(&output).unwrap().len();
    }
    let t_decoded = t1_start.elapsed().as_secs_f32();

    notify!("stop");

    println!(
        "encode {input}... to {output}...: {sum_encoded}, {t_encoded}",
        input = str::from_utf8(&input[..4]).unwrap(),
        output = &output[..4],
    );

    println!(
        "decode {output}... to {str3}...: {sum_decoded}, {t_decoded}",
        output = &output[..4],
        str3 = str::from_utf8(&str3[..4]).unwrap(),
    );
}
