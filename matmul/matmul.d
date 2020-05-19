// Originally written by Attractive Chaos; distributed under the MIT license (D V.2 code)
// Contributed by leonardo and then modified by Attractive Chaos to remove D 2.0 features

import std.numeric, std.stdio, std.string, std.conv;
import std.socket;
import std.compiler;
import std.format;
import std.math;
import core.stdc.stdlib;
import core.thread;

double[][] matGen(in int n) {
  auto len = n * n;
  double tmp = 1.0 / len;
  auto a = new double[][](n, n);
  size_t i;
  foreach (ref row; a)
  {
    sizediff_t u = i, v = i;
    foreach (ref x; row)
    {
      x = tmp * (u * v);
      u--;
      v++;
     }
     i++;
  }
  return a;
}

double[][] matMul(in double[][] a, in double[][] b) {
  ulong m = a.length, n = a[0].length, p = b[0].length;

  // transpose
  auto c = new double[][](p, n);
  foreach (i, brow; b)
    foreach (j, bx; brow)
      c[j][i] = bx;

  auto x = new double[][](m, p);

  foreach (i, arow; a)
    foreach (j, crow; c)
      x[i][j] = dotProduct(arow, crow);

  return x;
}

void notify(string msg) {
    try {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope(exit) socket.close();
        socket.send(msg);
    } catch (SocketOSException) {
        // standalone usage
    }
}

void main(in string[] args) {
  int n = 100;
  if (args.length >= 2) n = to!int(args[1]) / 2 * 2;

  auto t = matMul(matGen(100), matGen(100));
  if (abs(t[1][1] + 19.5) > 0.5) {
      exit(-1);
  }

  notify("%s\t%d".format(name, getpid()));

  auto a = matGen(n);
  auto b = matGen(n);
  auto x = matMul(a, b);
  printf("%.6f\n", x[n / 2][n / 2]);

  notify("stop");
}
