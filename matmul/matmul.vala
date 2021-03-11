//valac --pkg gio-2.0 --pkg posix -X -O3 -D GCC_TEST matmul.vala

class Prog
{
	static void notify(string msg)
	{
		try
		{
			var socket = new SocketClient();
			var conn = socket.connect_to_host("localhost",9001,null);
			conn.output_stream.write (msg.data);

		}
		catch (Error e)
		{
			// standalone usage
		}
	}
	
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
        var m = a.length[0];
        var n = a.length[1];
        var p = b.length[1];

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

    private static double Calc(int n) {
        n = n / 2 * 2;
        var a = MatGen(n, 1.0);
        var b = MatGen(n, 2.0);
        var x = MatMul(ref a, ref b);
        
        return x[n / 2, n / 2];
    }
    
    static void main(string[] args)
    {
        var n = (args.length > 1) ? (int.parse(args[1])) : 100;

        var left = Calc(101);
        var right = -18.67;
        if ((left - right).abs() > 0.1) {
            stderr.printf(@"$(left) != $(right)");
            Process.exit(1);
        }
		
		var msg = "Vala/";
		
		#if GCC_TEST
			msg += "gcc";
		#elif CLANG_TEST
			msg += "clang";
		#else
			// The preprocessor directive for the test should be specified
			Process.exit(-1);
		#endif
		msg += @"\t $((uint16)Posix.getpid())";

		notify(msg);
		
        var timer = new Timer();
        var results = Calc(n);
        
        timer.stop();

        notify("stop");

        stdout.printf(results.to_string()+"\n");
        stdout.printf(@"time: $(timer.elapsed())s\n");
    }

}

