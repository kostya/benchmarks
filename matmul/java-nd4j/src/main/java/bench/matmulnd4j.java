package bench;

import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.api.buffer.DataType;
import java.net.Socket;
import java.io.OutputStream;

final class matmulnd4j {

    private static INDArray matgen(final int n, final double seed) {
        final var idxs = Nd4j.linspace(DataType.DOUBLE, 0, n, 1);
        final var iIdxs = idxs.reshape(n,1);
        final var jIdxs = idxs.reshape(1, n);

        return (iIdxs.sub(jIdxs)).muli((iIdxs.add(jIdxs))).muli(seed / n / n);
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
        final var a = matgen(size, 1.0);
        final var b = matgen(size, 2.0);
        final var x = Nd4j.matmul(a, b);
        return x.getDouble(size / 2, size / 2);
    }

    public static void main(String[] args) {
        final var n = args.length > 0 ? Integer.valueOf(args[0]) : 100;

        final var left = calc(101);
        final var right = -18.67;
        if (Math.abs(left - right) > 0.5) {
            System.err.printf("%f != %f\n", left, right);
            System.exit(1);
        }

        notify("Java (ND4J)\t" + ProcessHandle.current().pid());
        final var start_time = System.currentTimeMillis();
        final var results = calc(n);
        final var time_diff = System.currentTimeMillis() - start_time;
        notify("stop");

        System.out.println(results);
        System.out.printf("time: %f s\n", time_diff / 1e3);
    }
}
