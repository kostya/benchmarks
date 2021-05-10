use std::collections::{HashSet, HashMap, VecDeque};

const UPPER_BOUND: i32 = 5000000;
const PREFIX: i32 = 32338;

#[derive(Debug)]
struct Node {
    children: HashMap<char, Box<Node>>,
    terminal: bool,
}

impl Node {
    fn new() -> Node {
        Node {
            children: HashMap::new(),
            terminal: false
        }
    }
}

fn generate_primes(m: i32) -> HashSet<i32> {
    let mut result: HashSet<i32> = [2].iter().cloned().collect();
    for i in 1..=(m - 1) / 2 {
        result.insert(2 * i + 1);
    }
    let (mut k, mut j) = (1, 1);
    let sqr = |i| i * i;
    let max_n = |i| (m - sqr(2 * i + 1)) / (4 * i + 2);
    while k > 0 {
        k = max_n(j);
        j += 1;
    }
    k = j;
    for i in 1..=k {
        for n in 0..max_n(i - 1) {
            result.remove(&((2 * i + 1) * (2 * i + 2 * n + 1)));
        }
    }
    result
}

fn generate_trie(l: HashSet<i32>) -> Box<Node> {
    let mut root: Box<Node> = Box::new(Node::new());
    for el in &l {
        let mut head = &mut root;
        for ch in el.to_string().chars() {
            if !head.children.contains_key(&ch) {
                head.children.insert(ch, Box::new(Node::new()));
            }
            head = head.children.get_mut(&ch).unwrap();
        }
        head.terminal = true;
    }
    root
}

fn find(upper_bound: i32, prefix: i32) -> Vec<i32> {
    let primes = generate_primes(upper_bound);
    let root = generate_trie(primes);
    let str_prefix = prefix.to_string();
    let mut head = &root;

    for ch in str_prefix.chars() {
        head = head.children.get(&ch).unwrap();
    }

    let mut queue: VecDeque<(&Box<Node>, String)> = VecDeque::new();
    queue.push_front((head, str_prefix));
    let mut result: Vec<i32> = Vec::new();
    while !queue.is_empty() {
        let (top, prefix) = queue.pop_back().unwrap();
        if top.terminal {
            result.push(prefix.parse().unwrap());
        }
        for (ch, v) in &top.children {
            let new_prefix = prefix.clone() + &ch.to_string();
            queue.push_front((&v, new_prefix));
        }
    }
    result
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).ok();
    }
}

fn verify() {
    let left = vec![2, 23, 29];
    let mut right = find(100, 2);
    right.sort();
    if left != right {
        eprintln!("{:?} != {:?}", left, right);
        std::process::exit(-1);
    }
}

fn main() {
    verify();

    notify(&format!("Rust\t{}", std::process::id()));
    let results = find(UPPER_BOUND, PREFIX);
    notify("stop");

    println!("{:?}", results);
}
