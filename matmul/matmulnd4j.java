import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;

class matmulnd4j {

	public static INDArray matgen(int n) {
		double[][] a = new double[n][n];
		double tmp = 1. / n / n;
		for (int i = 0; i < n; ++i)
			for (int j = 0; j < n; ++j)
				a[i][j] = tmp * (i - j) * (i + j);
		return Nd4j.create(a);
	}

	public static void main(String[] args) {
		int n = 100;
		if (args.length >= 1) n = Integer.parseInt(args[0]);
		n = n / 2 * 2;
		
		INDArray t =  Nd4j.matmul(matgen(500), matgen(500));
		System.out.println("JIT warming up: " + t.getDouble(1, 1));

                try (var socket = new java.net.Socket("localhost", 9001);
                     var out = socket.getOutputStream()) {
                    out.write("Java".getBytes("UTF-8"));
                } catch (java.io.IOException e) {
                    // standalone usage
                }

		long start_time = System.currentTimeMillis();

		INDArray a, b, x;
		a = matgen(n);
		b = matgen(n);
		x = Nd4j.matmul(a, b);
		System.out.println(x.getDouble(n/2, n/2));
		System.out.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");
	}
}
