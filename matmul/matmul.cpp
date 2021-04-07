#include <cstdlib>
#include <Eigen/Dense>
#include <iostream>
#include <libnotify.hpp>
#include <unistd.h>

using namespace std;
using namespace Eigen;

// NOTE: Eigen requires explicit variable types for the proper
// compile-time template processing

MatrixXd build_matrix(int n, double seed) {
  ArrayXXd i_idxs = ArrayXXd::Zero(n, n);
  i_idxs.colwise() = ArrayXd::LinSpaced(n, 0, n - 1);

  ArrayXXd j_idxs = ArrayXXd::Zero(n, n);
  j_idxs.rowwise() = ArrayXd::LinSpaced(n, 0, n - 1).transpose();

  ArrayXXd result = (i_idxs - j_idxs) * (i_idxs + j_idxs) * (seed / n / n);
  return result.matrix();
}

double calc(int n) {
  n = n / 2 * 2;
  MatrixXd a = build_matrix(n, 1.0);
  MatrixXd b = build_matrix(n, 2.0);
  MatrixXd d = a * b;
  return d(n / 2, n / 2);
}

int main(int argc, char** argv) {
  auto n = argc > 1 ? atoi(argv[1]) : 100;

  auto left = calc(101);
  auto right = -18.67;
  if (fabs(left - right) > 0.1) {
    cerr << left << " != " << right << endl;
    exit(EXIT_FAILURE);
  }

  stringstream ostr;
  ostr << "C++/g++ (Eigen)\t" << getpid();
  notify(ostr.str());

  auto results = calc(n);

  notify("stop");

  cout << results << endl;
}
