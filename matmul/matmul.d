// Originally written by Attractive Chaos; distributed under the MIT license (D V.2 code)
// Contributed by leonardo and then modified by Attractive Chaos to remove D 2.0 features

import std.numeric, std.stdio, std.string, std.conv;

double[][] matGen(in int n) {
  double tmp = 1.0 / n / n;
  auto a = new double[][](n, n);
  foreach (int i, row; a)
    foreach (int j, ref x; row)
      x = tmp * (i - j) * (i + j);
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

void main(in string[] args) {
  int n = 100;
  if (args.length >= 2) n = to!int(args[1]) / 2 * 2;
  auto a = matGen(n);
  auto b = matGen(n);
  auto x = matMul(a, b);
  printf("%.6f\n", x[n / 2][n / 2]);
}
