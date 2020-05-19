// Written by Attractive Chaos; distributed under the MIT license

class matmul {

    public double[][] matgen(int n) {
        double[][] a = new double[n][n];
        double tmp = 1. / n / n;
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < n; ++j)
                a[i][j] = tmp * (i - j) * (i + j);
        return a;
    }

    public double[][] matmul(double[][] a, double[][] b) {
        int m = a.length, n = a[0].length, p = b[0].length;
        double[][] x = new double[m][p];
        double[][] c = new double[p][n];
        for (int i = 0; i < n; ++i) // transpose
            for (int j = 0; j < p; ++j)
                c[j][i] = b[i][j];
        for (int i = 0; i < m; ++i)
            for (int j = 0; j < p; ++j) {
                double s = 0.0;
                for (int k = 0; k < n; ++k)
                    s += a[i][k] * c[j][k];
                x[i][j] = s;
            }
        return x;
    }

    private static void notify(final String msg) {
        try (var socket = new java.net.Socket("localhost", 9001);
             var out = socket.getOutputStream()) {
            out.write(msg.getBytes("UTF-8"));
        } catch (java.io.IOException e) {
            // standalone usage
        }
    }

    public static void main(String[] args) {
        int n = 100;
        if (args.length >= 1) n = Integer.parseInt(args[0]);
        n = n / 2 * 2;

        matmul m = new matmul();

        double[][] t = m.matmul(m.matgen(100), m.matgen(100));
        if (Math.abs(t[1][1] + 19.5) > 0.5) {
            System.exit(-1);
        }

        notify("Java\t" + ProcessHandle.current().pid());
        long start_time = System.currentTimeMillis();

        double[][] a, b, x;
        a = m.matgen(n);
        b = m.matgen(n);
        x = m.matmul(a, b);
        System.out.println(x[n/2][n/2]);
        System.out.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");

        notify("stop");
    }
}
