#include <xtensor/containers/xtensor.hpp>
#include <xtensor-blas/xlinalg.hpp>
#include <libnotify.h>
#include <cmath>
#include <iostream>

using namespace std;
using namespace xt;

#ifdef __clang__
static constexpr auto COMPILER = "clang++";
#else
static constexpr auto COMPILER = "g++";
#endif

xtensor<double, 2> build_matrix(int n, double seed) {
  const auto i_idxs = arange(n).reshape({-1,1});
  const auto j_idxs = arange(n).reshape({1,-1});

  return xtensor<double, 2>{(i_idxs - j_idxs) * (i_idxs + j_idxs) * (seed / n / n)};
}

double calc(int n) {
  n = n / 2 * 2;
  xtensor<double, 2> a = build_matrix(n, 1.0);
  xtensor<double, 2> b = build_matrix(n, 2.0);
  xtensor<double, 2> d = linalg::dot(a, b);
  return d(n / 2, n / 2);
}

int main(int argc, char **argv) {
  auto n = argc > 1 ? atoi(argv[1]) : 100;

  auto left = calc(101);
  auto right = -18.67;
  if (fabs(left - right) > 0.1) {
    cerr << left << " != " << right << endl;
    exit(EXIT_FAILURE);
  }

  auto results =
      notifying_invoke([&]() { return calc(n); }, "C++/{} (xtensor)", COMPILER);

  cout << results << endl;
}
