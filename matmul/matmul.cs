using System;
using System.Diagnostics;

class Prog
{
    static double[,] MatGen(int n, double seed)
    {
        var tmp = seed / n / n;
        var a = new double[n, n];
        for (var i = 0; i < n; ++i)
            for (var j = 0; j < n; ++j)
                a[i,j] = tmp * (i - j) * (i + j);
        return a;
    }

    static double[,] MatMul(ref double[,] a, ref double[,] b)
    {
        var m = a.GetLength(0);
        var n = a.GetLength(1);
        var p = b.GetLength(1);
        var x = new double[m, p];
        var c = new double[p, n];

        // transpose
        for (var i = 0; i < n; ++i)
            for (var j = 0; j < p; ++j)
                c[j,i] = b[i, j];

        for (var i = 0; i < m; ++i)
            for (var j = 0; j < p; ++j)
            {
                var s = 0.0;
                for (var k = 0; k < n; ++k)
                    s += a[i, k] * c[j, k];
                x[i, j] = s;
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

    private static double Calc(int n) {
        n = n / 2 * 2;
        var a = MatGen(n, 1.0);
        var b = MatGen(n, 2.0);
        var x = MatMul(ref a, ref b);
        return x[n / 2, n / 2];
    }

    static void Main(string[] args)
    {
        var n = args.Length > 0 ? int.Parse(args[0]) : 100;

        var left = Calc(101);
        var right = -18.67;
        if (Math.Abs(left - right) > 0.1) {
            Console.Error.WriteLine($"{left} != {right}");
            System.Environment.Exit(1);
        }

        var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
        Notify($"C#/{runtime}\t{Process.GetCurrentProcess().Id}");

        var sw = Stopwatch.StartNew();
        var results = Calc(n);
        sw.Stop();

        Notify("stop");

        Console.WriteLine(results);
        Console.WriteLine("time: {0}s", sw.Elapsed.TotalSeconds);
    }
}
