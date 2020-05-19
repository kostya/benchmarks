import numpy as np
import socket
import os

def build_matrix_np(n):
    i_idxs = np.atleast_2d(np.arange(n)).T
    j_idxs = np.atleast_2d(np.arange(n))
    return (i_idxs - j_idxs) * (i_idxs + j_idxs) * (1.0 / n / n)


def matmul(a, b):
    return np.dot(a, b)

def main(n):
    a = build_matrix_np(n)
    b = build_matrix_np(n)
    d = matmul(a, b)
    print(d[n // 2][n // 2])

def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, 'utf8'))

if __name__=='__main__':
    import sys

    n = 100
    if len(sys.argv) > 1:
        n = int(sys.argv[1]) * 2 // 2

    t = matmul(build_matrix_np(100), build_matrix_np(100))
    if abs(t[1][1] + 19.5) > 0.5:
        quit(-1)

    notify("Python NumPy\t%d" % (os.getpid()))

    main(n)

    notify("stop")
