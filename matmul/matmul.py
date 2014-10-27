# Writen by Attractive Chaos; distributed under the MIT license

import array
import sys

def matmul(a, b):
    c = [array.array("d", [0.0]) * len(b) for _ in xrange(len(b[0]))]
    for i in xrange(len(b[0])):
        for j in xrange(len(b)):
            c[i][j] = b[j][i]

    d = [array.array("d", [0.0]) * len(b[0]) for _ in xrange(len(a))]

    for i in xrange(len(a)):
        for j in xrange(len(b[0])):
            s, ai, cj = 0.0, a[i], c[j]
            for k in xrange(len(b)):
                s += ai[k] * cj[k]
            d[i][j] = s
    return d

def build_matrix(n):
    t = 1.0 / n / n
    m = [array.array("d", [0.0]) * n for _ in xrange(n)]
    for i in xrange(n):
        for j in xrange(n):
            m[i][j] = t * (i - j) * (i + j)
    return m

def main(argv):
    n = 100
    if len(sys.argv) > 1:
        n = int(sys.argv[1])

    a = build_matrix(n)
    b = build_matrix(n)

    d = matmul(a, b)
    print d[n // 2][n // 2]


if __name__ == "__main__":
    main(sys.argv)