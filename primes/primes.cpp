#include <algorithm>
#include <iostream>
#include <queue>
#include <sstream>
#include <unordered_map>
#include <vector>

#include <libnotify.h>

#ifdef __clang__
static constexpr auto COMPILER = "clang++";
#else
static constexpr auto COMPILER = "g++";
#endif

static const auto UPPER_BOUND = 5'000'000;
static const auto PREFIX = 32'338;

#include <memory_resource>
namespace pmr = std::pmr;

struct Node {
  using Map = pmr::unordered_map<char, Node *>;

  Map children;
  bool terminal = false;

  Node(pmr::monotonic_buffer_resource &alloc)
  : children(Map::allocator_type{&alloc})
  {}
};

class Sieve {
  int limit;
  std::vector<bool> prime;

  Sieve &omit_squares() {
    for (auto r = 5; r * r < limit; ++r) {
      if (prime[r]) {
        for (auto i = r * r; i < limit; i += r * r) {
          prime[i] = false;
        }
      }
    }
    return *this;
  }

  void step1(int x, int y) {
    const auto n = (4 * x * x) + (y * y);
    if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
      prime[n] = !prime[n];
    }
  }

  void step2(int x, int y) {
    const auto n = (3 * x * x) + (y * y);
    if (n <= limit && n % 12 == 7) {
      prime[n] = !prime[n];
    }
  }

  void step3(int x, int y) {
    const auto n = (3 * x * x) - (y * y);
    if (x > y && n <= limit && n % 12 == 11) {
      prime[n] = !prime[n];
    }
  }

  void loop_y(int x) {
    for (auto y = 1; y * y < limit; ++y) {
      step1(x, y);
      step2(x, y);
      step3(x, y);
    }
  }

  void loop_x() {
    for (auto x = 1; x * x < limit; ++x) {
      loop_y(x);
    }
  }

public:
  Sieve(int limit) : limit(limit), prime(limit + 1, false) {}

  std::vector<int> to_list() const {
    std::vector<int> result({2, 3});
    for (auto p = 5; p <= limit; ++p) {
      if (prime[p]) {
        result.push_back(p);
      }
    }
    return result;
  }

  Sieve &calc() {
    loop_x();
    return omit_squares();
  }
};

Node* make_node(pmr::monotonic_buffer_resource &alloc)
{
  return new(alloc.allocate(sizeof(Node), alignof(Node))) Node(alloc);
}

Node* generate_trie(pmr::monotonic_buffer_resource &alloc, const std::vector<int> &l) {
  auto root = make_node(alloc);
  for (const auto el : l) {
    auto* head = root;
    for (const auto ch : std::to_string(el)) {
      auto it = head->children.find(ch);
      if (it == head->children.end()) {
        it = head->children.emplace(ch, make_node(alloc)).first;
      }
      head = it->second;
    }
    head->terminal = true;
  }
  return root;
}

std::vector<int> find(pmr::monotonic_buffer_resource &&alloc, int upper_bound, int prefix) {
  const auto primes = Sieve(upper_bound).calc();
  const auto &str_prefix = std::to_string(prefix);
  auto head = generate_trie(alloc, primes.to_list());

  for (const auto ch : str_prefix) {
    head = head->children[ch];
  }

  std::queue<std::pair<Node*, std::string>> queue(
      { std::make_pair(head, str_prefix) }
  );
  std::vector<int> result;
  while (!queue.empty()) {
    const auto [top, prefix] = queue.front();
    queue.pop();
    if (top->terminal) {
      result.push_back(std::stoi(prefix));
    }
    for (const auto &[ch, v] : top->children) {
      queue.emplace(v, prefix + ch);
    }
  }
  std::sort(result.begin(), result.end());
  return result;
}

std::string to_string(const std::vector<int> &a) {
  std::stringstream ss;
  ss << "[";
  auto first = true;
  for (const auto el : a) {
    if (!first) {
      ss << ", ";
    }
    ss << el;
    first = false;
  }
  ss << "]";
  return ss.str();
}

void verify() {
  std::vector<int> left({2, 23, 29});
  auto right = find(pmr::monotonic_buffer_resource(), 100, 2);
  if (left != right) {
    std::cerr << to_string(left) << " != " << to_string(right) << std::endl;
    exit(EXIT_FAILURE);
  }
}

int main() {
  verify();

  const auto &results = notifying_invoke(
      [&]() { return find(pmr::monotonic_buffer_resource(), UPPER_BOUND, PREFIX); }, "C++/{}", COMPILER);

  std::cout << to_string(results) << std::endl;
}
