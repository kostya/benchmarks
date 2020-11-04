# Writen by Attractive Chaos; distributed under the MIT license

import array
import platform
import socket
import sys
import os


def matmul(a, b):
    c = [array.array("d", [0.0]) * len(b) for _ in range(len(b[0]))]
    for i in range(len(b[0])):
        for j in range(len(b)):
            c[i][j] = b[j][i]

    d = [array.array("d", [0.0]) * len(b[0]) for _ in range(len(a))]

    for i in range(len(a)):
        for j in range(len(b[0])):
            s, ai, cj = 0.0, a[i], c[j]
            for k in range(len(b)):
                s += ai[k] * cj[k]
            d[i][j] = s
    return d


def build_matrix(n, seed):
    t = seed / n / n
    m = [array.array("d", [0.0]) * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            m[i][j] = t * (i - j) * (i + j)
    return m


def calc(n):
    n = n // 2 * 2
    a = build_matrix(n, 1.0)
    b = build_matrix(n, 2.0)

    d = matmul(a, b)
    return d[n // 2][n // 2]


def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, "utf8"))


if __name__ == "__main__":
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 100

    left = calc(101)
    right = -18.67
    if abs(left - right) > 0.1:
        print("%f != %f" % (left, right), file=sys.stderr)
        quit(1)

    notify("%s\t%d" % (platform.python_implementation(), os.getpid()))
    results = calc(n)
    notify("stop")

    print(results)
