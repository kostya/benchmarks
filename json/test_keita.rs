use std::fs;
use std::str;
use keita_json::tree::{FlatTree, Node};

fn main() {
    let text = fs::read("1.json").expect("Failed to read 1.json");
    let tree = FlatTree::parse(&text).expect("Failed to parse JSON");

    let mut x_sum = 0.0;
    let mut y_sum = 0.0;
    let mut z_sum = 0.0;
    let mut count = 0;

    if let Some(Node::Array { start, len }) = tree.query("coordinates") {
        count = len;
        
        for i in 0..len {
            let child_idx = tree.elements[(start + i) as usize];
            let child_node = tree.nodes[child_idx as usize];

            if let Node::Object { start: obj_start, len: obj_len } = child_node {
                for j in 0..obj_len {
                    let (k_start, k_end, val_idx) = tree.kvs[(obj_start + j) as usize];
                    let key = unsafe { str::from_utf8_unchecked(&tree.raw[k_start as usize..k_end as usize]) };
                    
                    if key == "x" || key == "y" || key == "z" {
                        let val_node = tree.nodes[val_idx as usize];
                        
                        if let Node::Number { start: n_start, end: n_end } = val_node {
                            let num_str = unsafe { str::from_utf8_unchecked(&tree.raw[n_start as usize..n_end as usize]) };
                            let val: f64 = num_str.parse().unwrap();
                            
                            match key {
                                "x" => x_sum += val,
                                "y" => y_sum += val,
                                "z" => z_sum += val,
                                _ => unreachable!(),
                            }
                        }
                    }
                }
            }
        }
    }

    println!("{} {} {} {}", x_sum, y_sum, z_sum, count);
}
