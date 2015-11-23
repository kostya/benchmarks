package bench;

import com.dslplatform.json.*;
import java.io.*;
import java.util.*;

public class TestJava {

	@CompiledJson
	public static class Root {
		private List<Model> c;
		public List<Model> getCoordinates() { return c; }
		public void setCoordinates(List<Model> v) { c = v; }
	}

	@CompiledJson
	public static class Model {
		private double x, y, z;
		public double getX() { return x; }
		public double getY() { return y; }
		public double getZ() { return z; }
		public void setX(double v) { x = v; }
		public void setY(double v) { y = v; }
		public void setZ(double v) { z = v; }
	}

	public static void parse(String filename) throws IOException {
		long start_time = System.currentTimeMillis();
		FileInputStream fis = new FileInputStream(filename);
		DslJson<Object> json = new DslJson<Object>();
		Root result = json.deserialize(Root.class, fis, new byte[8196]);
		double x = 0, y = 0, z = 0;
		int total = result.getCoordinates().size();
		for(Model m : result.getCoordinates()) {
			x += m.getX();
			y += m.getY();
			z += m.getZ();
		}
		System.out.println(x / total);
		System.out.println(y / total);
		System.out.println(z / total);
		System.out.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");
	}

	public static void main(String[] args) throws IOException {
		// warming
		for(int i = 0; i < 3; i++) {
			parse("1.json");
		}
	}
}

