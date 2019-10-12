void main (string[] args) {
	int n = 100;
	if (args.length > 1) {
		n = int.parse(args[1]);
	}
	n = n / 2 * 2;
	double[,] a = matgen(n);
	double[,] b = matgen(n);
	double[,] x = matmul(a, b);
	print("%f\n", x[n/2,n/2]);
}

double[,] matgen (int n) {
	double[,] arr = new double[n,n];
	double tmp = 1.0 / n / n;
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < n; ++j) {
			arr[i,j] = tmp * (i - j) * (i + j);
		}
	}
	return arr;
}

double[,] matmul (double[,] a, double[,] b) {
	int m = a.length[0];
	int n = a.length[1];
	int p = b.length[1];
	double[,] x = new double[m,p];
	double[,] c = new double[p,n];
	for (int i = 0; i < n; ++i) { // transpose
		for (int j = 0; j < p; ++j) {
			c[j,i] = b[i,j];
		}
	}
	for (int i = 0; i < m; ++i) {
		for (int j = 0; j < p; ++j) {
			double s = 0.0;
			for (int k = 0; k < n; ++k) {
				s += a[i,k] * c[j,k];
			}
			x[i,j] = s;
		}
	}
	return x;
}
