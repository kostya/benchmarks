// Base64: encoding a large blob, then decoding its text, each many times over
// into a newly allocated buffer, with the standard library's codec.

import std;
import os;
import base64;

extern "goose_notify_start" fn notify_start(variant: const u8[:]);
extern "goose_notify_stop" fn notify_stop();

let STR_SIZE = 131072;
let TRIES = 8192;

struct Fixture { src: const u8[:], dst: const u8[:] }

fn main() {
    for f in [Fixture { "hello", "aGVsbG8=" }, Fixture { "world", "d29ybGQ=" }] {
        var encoded: u8[>..] = [];
        base64::encode(encoded, f.src);
        if encoded != f.dst { abort(str(encoded, " != ", f.dst)); }
        var decoded: u8[>..] = [];
        if !base64::decode(decoded, f.dst) || decoded != f.src { abort(str(decoded, " != ", f.src)); }
    }

    var input: u8[>..] = [];
    input.resize(STR_SIZE, 'a');
    var output: u8[>..] = [];
    base64::encode(output, input);
    var str3: u8[>..] = [];
    if !base64::decode(str3, output) { abort("invalid base64"); }

    notify_start("");

    // Every buffer is new: each iteration's is gone again at its closing
    // brace, which is all freeing it takes.
    let t_start = clock();
    var sum_encoded = 0;
    for i in TRIES {
        var encoded: u8[>..] = [];
        base64::encode(encoded, input);
        sum_encoded += encoded.len;
    }
    let t_encoded = clock() - t_start;

    let t1_start = clock();
    var sum_decoded = 0;
    for i in TRIES {
        var decoded: u8[>..] = [];
        if !base64::decode(decoded, output) { abort("invalid base64"); }
        sum_decoded += decoded.len;
    }
    let t_decoded = clock() - t1_start;

    notify_stop();

    print("encode ", input[..4], "... to ", output[..4], "...: ", sum_encoded, ", ", t_encoded);
    print("decode ", output[..4], "... to ", str3[..4], "...: ", sum_decoded, ", ", t_decoded);
}
