import os
import platform
import socket
import sys


UPPER_BOUND = 5000000
PREFIX = 32338


class Node:
    def __init__(self):
        self.children = {}
        self.terminal = False


def generate_primes(m):
    result = {2}
    for i in range(1, 1 + (m - 1) // 2):
        result.add(2 * i + 1)
    k, j = 1, 1
    sqr = lambda i: i * i
    max_n = lambda i: (m - sqr(2 * i + 1)) // (4 * i + 2)
    while k > 0:
        k = max_n(j)
        j += 1
    k = j
    for i in range(1, k + 1):
        for n in range(max_n(i - 1)):
            result.discard((2 * i + 1) * (2 * i + 2 * n + 1))
    return result


def generate_trie(l):
    root = Node()
    for el in l:
        head = root
        for ch in str(el):
            head = head.children.setdefault(ch, Node())
        head.terminal = True
    return root


def find(upper_bound, prefix):
    primes = generate_primes(upper_bound)
    root = generate_trie(primes)
    head, str_prefix = root, str(prefix)
    for ch in str_prefix:
        head = head.children.get(ch)
        if head is None:
            return None

    queue, result = [(head, str_prefix)], []
    while queue:
        top, prefix = queue.pop()
        if top.terminal:
            result.append(int(prefix))
        for ch, v in top.children.items():
            queue.insert(0, (v, prefix + ch))
    return result


def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, "utf8"))


def verify():
    left = [2, 23, 29]
    right = find(100, 2)
    if left != right:
        print("%s != %s" % (left, right), file=sys.stderr)
        quit(1)


if __name__ == "__main__":
    verify()

    notify("%s\t%d" % (platform.python_implementation(), os.getpid()))
    results = find(UPPER_BOUND, PREFIX)
    notify("stop")

    print(results)
