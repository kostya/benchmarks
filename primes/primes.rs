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
            terminal: false,
        }
    }
}

struct Sieve {
    limit: usize,
    prime: Vec<bool>,
}

impl Sieve {
    fn new(limit: usize) -> Sieve {
        Sieve {
            limit: limit,
            prime: vec![false; limit + 1],
        }
    }

    fn to_list(&self) -> Vec<usize> {
        let mut result = vec![2, 3];
        for p in 5..=self.limit {
            if self.prime[p] {
                result.push(p);
            }
        }
        result
    }

    fn omit_squares(&mut self) -> &Self {
        let mut r = 5;
        while r * r < self.limit {
            if self.prime[r] {
                let mut i = r * r;
                while i < self.limit {
                    self.prime[i] = false;
                    i = i + r * r;
                }
            }
            r = r + 1;
        }
        self
    }

    fn step1(&mut self, x: usize, y: usize) {
        let n = (4 * x * x) + (y * y);
        if n <= self.limit && (n % 12 == 1 || n % 12 == 5) {
            self.prime[n] = !self.prime[n];
        }
    }

    fn step2(&mut self, x: usize, y: usize) {
        let n = (3 * x * x) + (y * y);
        if n <= self.limit && n % 12 == 7 {
            self.prime[n] = !self.prime[n];
        }
    }

    fn step3(&mut self, x: usize, y: usize) {
        let n = (3 * x * x) - (y * y);
        if x > y && n <= self.limit && n % 12 == 11 {
            self.prime[n] = !self.prime[n];
        }
    }

    fn loop_y(&mut self, x: usize) {
        let mut y = 1;
        while y * y < self.limit {
            self.step1(x, y);
            self.step2(x, y);
            self.step3(x, y);
            y += 1;
        }
    }

    fn loop_x(&mut self) {
        let mut x = 1;
        while x * x < self.limit {
            self.loop_y(x);
            x += 1;
        }
    }

    fn calc(&mut self) -> &Self {
        self.loop_x();
        self.omit_squares()
    }
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
    let mut sieve = Sieve::new(upper_bound);
    let primes = sieve.calc();
    let str_prefix = prefix.to_string();
    let mut head = &generate_trie(primes.to_list());

    for ch in str_prefix.chars() {
        head = head.children.get(&ch).unwrap();
    }

    let mut queue: VecDeque<(&Box<Node>, String)> = VecDeque::new();
    queue.push_front((&head, str_prefix));
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
