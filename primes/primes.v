import net

const upper_bound = usize(5_000_000)
const prefix = 32_338

struct Node {
mut:
	children map[u8]&Node
	terminal bool
}

struct Sieve {
mut:
	limit usize
	prime []bool
}

fn (s Sieve) to_list() []usize {
	mut result := [usize(2), 3]
	for p := usize(5); p <= s.limit; p++ {
		if s.prime[p] {
			result << p
		}
	}
	return result
}

fn (mut s Sieve) omit_squares() {
	for r := 5; r * r < s.limit; r++ {
		if s.prime[r] {
			for i := r * r; i < s.limit; i += r * r {
				s.prime[i] = false
			}
		}
	}
}

fn (mut s Sieve) step1(x usize, y usize) {
	n := (4 * x * x) + (y * y)
	if n <= s.limit && (n % 12 == 1 || n % 12 == 5) {
		s.prime[n] = !s.prime[n]
	}
}

fn (mut s Sieve) step2(x usize, y usize) {
	n := (3 * x * x) + (y * y)
	if n <= s.limit && n % 12 == 7 {
		s.prime[n] = !s.prime[n]
	}
}

fn (mut s Sieve) step3(x usize, y usize) {
	n := (3 * x * x) - (y * y)
	if x > y && n <= s.limit && n % 12 == 11 {
		s.prime[n] = !s.prime[n]
	}
}

fn (mut s Sieve) loop_y(x usize) {
	for y := usize(1); y * y < s.limit; y++ {
		s.step1(x, y)
		s.step2(x, y)
		s.step3(x, y)
	}
}

fn (mut s Sieve) loop_x() {
	for x := usize(1); x * x < s.limit; x++ {
		s.loop_y(x)
	}
}

fn (mut s Sieve) calc() {
	s.loop_x()
	s.omit_squares()
}

fn generate_trie(l []usize) Node {
	mut root := Node{}
	for el in l {
		mut head := &root
		for ch in el.str().bytes() {
			if ch !in head.children {
				head.children[ch] = &Node{}
			}
			head = head.children[ch] or { panic('key not found') }
		}
		head.terminal = true
	}
	return root
}

struct Pair {
	Node
	p_prefix string
}

fn find(upper_bound usize, prefix int) []int {
	mut primes := Sieve{upper_bound, []bool{len: int(upper_bound + 1)}}
	primes.calc()
	str_prefix := prefix.str()
	mut head := generate_trie(primes.to_list())
	for ch in str_prefix.bytes() {
		head = *head.children[ch] or { panic('key not found') }
	}
	mut queue := []Pair{}
	queue.prepend(Pair{head, str_prefix})
	mut result := []int{}
	for queue.len > 0 {
		pair := queue.pop()
		if pair.terminal {
			result << pair.p_prefix.int()
		}
		for ch, v in pair.children {
			queue.insert(0, Pair{v, pair.p_prefix + ch.ascii_str()})
		}
	}
	result.sort()
	return result
}

fn notify(msg string) {
	mut sock := net.dial_tcp('127.0.0.1:9001') or { return }
	defer {
		sock.close() or {}
	}
	sock.write_string(msg) or {}
}

fn verify() {
	left := [2, 23, 29]
	right := find(100, 2)
	if left != right {
		panic('${left} != ${right}')
	}
}

fn main() {
	verify()
	mut lang := 'V/gcc'
	$if clang {
		lang = 'V/clang'
	}
	notify('${lang}\t${C.getpid()}')
	results := find(upper_bound, prefix)
	notify('stop')
	println(results)
}
