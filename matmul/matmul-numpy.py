import numpy as np
import socket
import os


def build_matrix_np(n, seed):
    i_idxs = np.atleast_2d(np.arange(n)).T
    j_idxs = np.atleast_2d(np.arange(n))
    return (i_idxs - j_idxs) * (i_idxs + j_idxs) * (seed / n / n)


def matmul(a, b):
    return np.dot(a, b)


def calc(n):
    n = n // 2 * 2
    a = build_matrix_np(n, 1.0)
    b = build_matrix_np(n, 2.0)
    d = matmul(a, b)
    return d[n // 2][n // 2]


def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, "utf8"))


if __name__ == "__main__":
    import sys

    n = int(sys.argv[1]) if len(sys.argv) > 1 else 100

    left = calc(101)
    right = -18.67
    if abs(left - right) > 0.1:
        print("%f != %f" % (left, right), file=sys.stderr)
        quit(1)

    notify("Python (NumPy)\t%d" % (os.getpid()))
    results = calc(n)
    notify("stop")

    print(results)
