using System;

namespace test
{
    class Prog
    {
        static double[,] MatGen(int n)
        {
            double tmp = 1.0 / n / n;
            var a = new double[n, n];
            for (int i = 0; i < n; ++i)
                for (int j = 0; j < n; ++j)
                    a[i,j] = tmp * (i - j) * (i + j);
            return a;
        }

        static double[,] MatMul(ref double[,] a, ref double[,] b)
        {
            int m = a.GetLength(0), n = a.GetLength(1), p = b.GetLength(1);
            var x = new double[m,p];
            var c = new double[p,n];

            // transpose
            for (int i = 0; i < n; ++i)
                for (int j = 0; j < p; ++j)
                    c[j,i] = b[i,j];

            for (int i = 0; i < m; ++i)
                for (int j = 0; j < p; ++j)
                {
                    double s = 0.0;
                    for (int k = 0; k < n; ++k)
                        s += a[i,k] * c[j,k];
                    x[i,j] = s;
                }

            return x;
        }

        static void Main(string[] args)
        {
            int n = 100;
            if (args.Length >= 1) n = int.Parse(args[0]) / 2 * 2;
            Console.WriteLine("N = {0}", n);
            double[,] a, b, x;
            a = MatGen(n);
            b = MatGen(n);
            x = MatMul(ref a, ref b);
            Console.WriteLine(x[n/2,n/2]);
        }
    }
}
