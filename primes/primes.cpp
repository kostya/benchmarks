#include <iostream>
#include <libnotify.hpp>
#include <memory>
#include <queue>
#include <sstream>
#include <unistd.h>
#include <unordered_map>
#include <utility>
#include <vector>

const auto UPPER_BOUND = 5'000'000;
const auto PREFIX = 32'338;

struct Node {
  std::unordered_map<char, std::shared_ptr<Node>> children;
  bool terminal = false;
};

std::vector<int> generate_primes(int limit) {
  std::vector<bool> prime(limit + 1, false);

  for (auto x = 1; x * x < limit; ++x) {
    for (auto y = 1; y * y < limit; ++y) {
      auto n = (4 * x * x) + (y * y);
      if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
        prime[n] = !prime[n];
      }

      n = (3 * x * x) + (y * y);
      if (n <= limit && n % 12 == 7) {
        prime[n] = !prime[n];
      }

      n = (3 * x * x) - (y * y);
      if (x > y && n <= limit && n % 12 == 11) {
        prime[n] = !prime[n];
      }
    }
  }

  for (auto r = 5; r * r < limit; ++r) {
    if (prime[r]) {
      for (auto i = r * r; i < limit; i += r * r) {
        prime[i] = false;
      }
    }
  }

  std::vector<int> result({2, 3});
  for (auto p = 5; p <= limit; ++p) {
    if (prime[p]) {
      result.push_back(p);
    }
  }
  return result;
}

std::shared_ptr<Node> generate_trie(const std::vector<int>& l) {
  std::shared_ptr<Node> root(new Node);
  for (const auto el : l) {
    auto head = root;
    for (const auto ch: std::to_string(el)) {
      head->children.emplace(ch, std::shared_ptr<Node>(new Node));
      head = head->children[ch];
    }
    head->terminal = true;
  }
  return root;
}

std::vector<int> find(int upper_bound, int prefix) {
  const auto primes = generate_primes(upper_bound);
  const auto root = generate_trie(primes);
  const auto& str_prefix = std::to_string(prefix);
  auto head = root;

  for (const auto ch : str_prefix) {
    head = head->children.at(ch);
  }

  std::queue<std::pair<std::shared_ptr<Node>, std::string>> queue(
    { std::make_pair(head, str_prefix) }
  );
  std::vector<int> result;
  while (!queue.empty()) {
    const auto& [top, prefix] = queue.front();
    queue.pop();
    if (top->terminal) {
      result.push_back(std::stoi(prefix));
    }
    for (const auto& [ch, v] : top->children) {
      queue.push(std::make_pair(v, prefix + ch));
    }
  }
  return result;
}

std::string to_string(std::vector<int> a) {
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
  auto right = find(100, 2);
  std::sort(right.begin(), right.end());
  if (left != right) {
    std::cerr << to_string(left) << " != " << to_string(right) << std::endl;
    exit(EXIT_FAILURE);
  }
}

int main() {
  verify();

  std::stringstream ostr;
  ostr << "C++/g++\t" << getpid();
  notify(ostr.str());

  const auto& results = find(UPPER_BOUND, PREFIX);

  notify("stop");

  std::cout << to_string(results) << std::endl;
}
