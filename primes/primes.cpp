#include <iostream>
#include <libnotify.hpp>
#include <memory>
#include <queue>
#include <sstream>
#include <unistd.h>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

const auto UPPER_BOUND = 5000000;
const auto PREFIX = 32338;

struct Node {
  std::unordered_map<char, std::shared_ptr<Node>> children;
  bool terminal = false;
};

std::unordered_set<int> generate_primes(int m) {
  std::unordered_set<int> result({2});
  for (auto i = 1; i < 1 + (m - 1) / 2; ++i) {
    result.insert(2 * i + 1);
  }
  auto k = 1, j = 1;
  const auto sqr = [m](int i) { return i * i; };
  const auto max_n = [m, sqr](int i) {
    return (m - sqr(2 * i + 1)) / (4 * i + 2);
  };

  while (k > 0) {
    k = max_n(j++);
  }
  k = j;
  for (auto i = 1; i < k + 1; ++i) {
    for (auto n = 0; n < max_n(i - 1); ++n) {
      result.erase((2 * i + 1) * (2 * i + 2 * n + 1));
    }
  }
  return result;
}

std::shared_ptr<Node> generate_trie(const std::unordered_set<int>& l) {
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
  std::stringstream ss("[");
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
  const auto& right = find(100, 2);
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
