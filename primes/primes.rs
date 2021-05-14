use std::collections::{HashMap, VecDeque};

const UPPER_BOUND: usize = 5_000_000;
const PREFIX: i32 = 32_338;

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

fn generate_primes(limit: usize) -> Vec<usize> {
    let mut prime = vec![false; limit + 1];

    let mut x = 1;
    while x * x < limit {
        let mut y = 1;
        while y * y < limit {
            let mut n = (4 * x * x) + (y * y);
            if n <= limit && (n % 12 == 1 || n % 12 == 5) {
                prime[n] = !prime[n];
            }

            n = (3 * x * x) + (y * y);
            if n <= limit && n % 12 == 7 {
                prime[n] = !prime[n];
            }

            n = (3 * x * x) - (y * y);
            if x > y && n <= limit && n % 12 == 11 {
                prime[n] = !prime[n];
            }
            y = y + 1;
        }
        x = x + 1;
    }

    let mut r = 5;
    while r * r < limit {
        if prime[r] {
            let mut i = r * r;
            while i < limit {
                prime[i] = false;
                i = i + r * r;
            }
        }
        r = r + 1;
    }

    let mut result = vec![2, 3];
    for p in 5..=limit {
        if prime[p] {
            result.push(p);
        }
    }
    result
}

fn generate_trie(l: Vec<usize>) -> Box<Node> {
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

fn find(upper_bound: usize, prefix: i32) -> Vec<i32> {
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
