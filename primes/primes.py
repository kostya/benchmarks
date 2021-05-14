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


def generate_primes(limit):
    prime = [False] * (limit + 1)

    x = 1
    while x * x < limit:
        y = 1
        while y * y < limit:
            n = (4 * x * x) + (y * y)
            if n <= limit and (n % 12 == 1 or n % 12 == 5):
                prime[n] = not prime[n]

            n = (3 * x * x) + (y * y)
            if n <= limit and n % 12 == 7:
                prime[n] = not prime[n]

            n = (3 * x * x) - (y * y)
            if x > y and n <= limit and n % 12 == 11:
                prime[n] = not prime[n]
            y = y + 1
        x = x + 1

    r = 5
    while r * r < limit:
        if prime[r]:
            i = r * r
            while i < limit:
                prime[i] = False
                i = i + r * r
        r = r + 1

    result = [2, 3]
    for p in range(5, limit + 1):
        if prime[p]:
            result.append(p)
    return result


def generate_trie(l):
    root = Node()
    for el in l:
        head = root
        for ch in str(el):
            if ch not in head.children:
                head.children[ch] = Node()
            head = head.children[ch]
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
