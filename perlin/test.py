# encoding: utf-8

from random import Random
import math
import sys
import time

def lerp(a, b, v):
    return a * (1 - v) + b * v

def smooth(v):
    return v * v * (3 - 2 * v)

def random_gradient(r):
    v = r.random() * math.pi * 2.0
    return Vec2(math.cos(v), math.sin(v))

def gradient(orig, grad, p):
    sp = Vec2(p.x - orig.x, p.y - orig.y)
    return grad.x * sp.x + grad.y * sp.y

class Vec2(object):
    __slots__ = ('x', 'y')

    def __init__(self, x, y):
        self.x = x
        self.y = y

class Noise2DContext(object):
    __slots__ = ('rgradients', 'permutations', 'gradients', 'origins')

    def __init__(self, seed):
        self.rgradients = []
        self.permutations = []
        self.gradients = [None, None, None, None]
        self.origins = [None, None, None, None]

        r = Random(seed)
        for i in xrange(256):
            self.rgradients.append(random_gradient(r))

        for i in xrange(256):
            self.permutations.append(i)
        r.shuffle(self.permutations)

    def get_gradient(self, x, y):
        idx = self.permutations[x & 255] + self.permutations[y & 255]
        return self.rgradients[idx & 255]

    def get_gradients(self, x, y):
        x0f = math.floor(x)
        y0f = math.floor(y)
        x0 = int(x0f)
        y0 = int(y0f)
        x1 = x0 + 1
        y1 = y0 + 1

        self.gradients[0] = self.get_gradient(x0, y0)
        self.gradients[1] = self.get_gradient(x1, y0)
        self.gradients[2] = self.get_gradient(x0, y1)
        self.gradients[3] = self.get_gradient(x1, y1)

        self.origins[0] = Vec2(x0f + 0.0, y0f + 0.0)
        self.origins[1] = Vec2(x0f + 1.0, y0f + 0.0)
        self.origins[2] = Vec2(x0f + 0.0, y0f + 1.0)
        self.origins[3] = Vec2(x0f + 1.0, y0f + 1.0)

    def get(self, x, y):
        p = Vec2(x, y)
        self.get_gradients(x, y)
        v0 = gradient(self.origins[0], self.gradients[0], p)
        v1 = gradient(self.origins[1], self.gradients[1], p)
        v2 = gradient(self.origins[2], self.gradients[2], p)
        v3 = gradient(self.origins[3], self.gradients[3], p)

        fx = smooth(x - self.origins[0].x)
        vx0 = lerp(v0, v1, fx)
        vx1 = lerp(v2, v3, fx)
        fy = smooth(y - self.origins[0].y)
        return lerp(vx0, vx1, fy)

def main(argv):
    symbols = [' ', '░', '▒', '▓', '█', '█']
    pixels = [['' for i in xrange(256)] for i in xrange(256)]
    n2d = Noise2DContext(time.time())

    n = 1
    if len(sys.argv) > 1:
        n = int(sys.argv[1])

    for i in xrange(n):
        for y in xrange(256):
            for x in xrange(256):
                v = n2d.get(x * 0.1, (y + (i * 128)) * 0.1) * 0.5 + 0.5
                s = symbols[int(v / 0.2)]
                pixels[y][x] = s

    for y in xrange(256):
        for x in xrange(256):
            sys.stdout.write(pixels[y][x])
        sys.stdout.write('\n')

if __name__ == "__main__":
    main(sys.argv)
