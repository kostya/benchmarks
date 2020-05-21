// Written by Attractive Chaos; distributed under the MIT license

final class matmul {

    private static double[][] matgen(final int n) {
        final var a = new double[n][n];
        final var tmp = 1. / n / n;
        for (var i = 0; i < n; ++i)
            for (var j = 0; j < n; ++j)
                a[i][j] = tmp * (i - j) * (i + j);
        return a;
    }

    private static double[][] matmul(final double[][] a, final double[][] b) {
        final var m = a.length;
        final var n = a[0].length;
        final var p = b[0].length;
        final var x = new double[m][p];
        final var c = new double[p][n];
        for (var i = 0; i < n; ++i) // transpose
            for (var j = 0; j < p; ++j)
                c[j][i] = b[i][j];
        for (var i = 0; i < m; ++i)
            for (var j = 0; j < p; ++j) {
                var s = 0.0;
                for (var k = 0; k < n; ++k)
                    s += a[i][k] * c[j][k];
                x[i][j] = s;
            }
        return x;
    }

    private static void notify(final String msg) {
        try (final var socket = new java.net.Socket("localhost", 9001);
             final var out = socket.getOutputStream()) {
            out.write(msg.getBytes("UTF-8"));
        } catch (final java.io.IOException e) {
            // standalone usage
        }
    }

    private static double calc(final int n) {
        final var size = n / 2 * 2;
        final var a = matgen(size);
        final var b = matgen(size);
        final var x = matmul(a, b);
        return x[size / 2][size / 2];
    }

    public static void main(String[] args) {
        final var n = args.length > 0 ? Integer.valueOf(args[0]) : 100;

        var left = calc(101);
        var right = -9.34;
        if (Math.abs(left - right) > 0.1) {
            System.err.printf("%f != %f\n", left, right);
            System.exit(1);
        }

        notify("Java\t" + ProcessHandle.current().pid());
        long start_time = System.currentTimeMillis();

        System.out.println(calc(n));
        final var time_diff = System.currentTimeMillis() - start_time;
        System.out.printf("time: %f s\n", time_diff / 1e3);

        notify("stop");
    }
}
