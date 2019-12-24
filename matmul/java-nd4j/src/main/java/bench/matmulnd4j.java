package bench;

import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;
import org.nd4j.linalg.api.buffer.DataType;
import java.net.Socket;
import java.io.OutputStream;

class matmulnd4j {

	public static INDArray matgen(int n) {
		INDArray idxs = Nd4j.linspace(DataType.DOUBLE, 0, n, 1);
		INDArray iIdxs = idxs.reshape(n,1);
		INDArray jIdxs = idxs.reshape(1, n);

		return (iIdxs.sub(jIdxs)).muli((iIdxs.add(jIdxs))).muli(1.0/n/n);
	}

	public static void main(String[] args) {
		int n = 100;
		if (args.length >= 1) n = Integer.parseInt(args[0]);
		n = n / 2 * 2;
		
		INDArray t =  Nd4j.matmul(matgen(500), matgen(500));
		System.out.println("JIT warming up: " + t.getDouble(1, 1));
		
		try (Socket socket = new Socket("localhost", 9001);
			OutputStream out = socket.getOutputStream()) {
			out.write("Java ND4J".getBytes("UTF-8"));
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
