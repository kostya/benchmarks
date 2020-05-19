using System;
using System.Diagnostics;

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

        private static void Notify(string msg) {
            try {
                using (var s = new System.Net.Sockets.TcpClient("localhost", 9001)) {
                    var data = System.Text.Encoding.UTF8.GetBytes(msg);
                    s.Client.Send(data);
                }
            } catch {
                // standalone usage
            }
        }

        static void Main(string[] args)
        {
            int n = 100;
            if (args.Length >= 1) n = int.Parse(args[0]) / 2 * 2;

            var t1 = MatGen(100);
            var t2 = MatGen(100);
            var t = MatMul(ref t1, ref t2);
            if (Math.Abs(t[1, 1] + 19.5) > 0.5) {
                System.Environment.Exit(-1);
            }

            var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
            Notify($"C# {runtime}\t{Process.GetCurrentProcess().Id}");

            var sw = Stopwatch.StartNew();
            var a = MatGen(n);
            var b = MatGen(n);
            var x = MatMul(ref a, ref b);
            sw.Stop();
            Console.WriteLine(x[n/2,n/2]);
            Console.WriteLine("time: {0}s", sw.Elapsed.TotalSeconds);

            Notify("stop");
        }
    }
}
