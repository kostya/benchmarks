extern crate serde_codegen;

use std::env;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();

    let src_pull = Path::new("src/serde_types_pull.in.rs");
    let dst_pull = Path::new(&out_dir).join("serde_types_pull.rs");

    serde_codegen::expand(&src_pull, &dst_pull).unwrap();
    
    let src_struct = Path::new("src/serde_types_struct.in.rs");
    let dst_struct = Path::new(&out_dir).join("serde_types_struct.rs");

    serde_codegen::expand(&src_struct, &dst_struct).unwrap();
} 
