import java.util.Random;

public class test {
	public static class Vec2 {
		public Vec2(float x, float y) {
			this.x = x;
			this.y = y;
		}
		
		public float x;
		public float y;
	}
	
	private static class Noise2DContext {
		Vec2[] rgradients;
		int[] permutations;
		Vec2[] gradients;
		Vec2[] origins;
		
		private static final float lerp(float a, float b, float v) {
			return a * (1 - v) + b * v;
		}
		
		private static final float smooth(float v) {
			return v * v * (3 - 2 * v);
		}
		
		Vec2 random_gradient(Random rnd) {
			double v = rnd.nextDouble() * Math.PI * 2.0;
			return new Vec2((float) Math.cos(v), (float) Math.sin(v));
		}
		
		float gradient(Vec2 orig, Vec2 grad, Vec2 p) {
			Vec2 sp = new Vec2(p.x - orig.x, p.y - orig.y);
			return grad.x * sp.x + grad.y * sp.y;
		}
		
		public Noise2DContext(int seed) {
			Random rnd = new Random(seed);
			rgradients = new Vec2[256];
			permutations = new int[256];
			for (int i = 0; i < 256; i++) {
				rgradients[i] = random_gradient(rnd);
			}
			for (int i = 0; i < 256; i++) {
				int j = rnd.nextInt(i + 1);
				permutations[i] = permutations[j];
				permutations[j] = i;
			}
			
			gradients = new Vec2[4];
			origins = new Vec2[4];
		}
		
		Vec2 get_gradient(int x, int y) {
			int idx = permutations[x & 255] + permutations[y & 255];
			return rgradients[idx & 255];
		}
		
		void get_gradients(float x, float y) {
			float x0f = (float) Math.floor(x);
			float y0f = (float) Math.floor(y);
			int x0 = (int) x0f;
			int y0 = (int) y0f;
			int x1 = x0 + 1;
			int y1 = y0 + 1;
			
			gradients[0] = get_gradient(x0, y0);
			gradients[1] = get_gradient(x1, y0);
			gradients[2] = get_gradient(x0, y1);
			gradients[3] = get_gradient(x1, y1);
			
			origins[0] = new Vec2(x0f + 0, y0f + 0);
			origins[1] = new Vec2(x0f + 1, y0f + 0);
			origins[2] = new Vec2(x0f + 0, y0f + 1);
			origins[3] = new Vec2(x0f + 1, y0f + 1);
		}
		
		public float get(float x, float y) {
			Vec2 p = new Vec2(x, y);
			get_gradients(x, y);
			float v0 = gradient(origins[0], gradients[0], p);
			float v1 = gradient(origins[1], gradients[1], p);
			float v2 = gradient(origins[2], gradients[2], p);
			float v3 = gradient(origins[3], gradients[3], p);
			
			float fx = smooth(x - origins[0].x);
			float vx0 = lerp(v0, v1, fx);
			float vx1 = lerp(v2, v3, fx);
			float fy = smooth(y - origins[0].y);
			return lerp(vx0, vx1, fy);
		}
	}
	
	static char[] symbols = { ' ', '░', '▒', '▓', '█', '█' };
	
	public test(int n) {
		Noise2DContext n2d = new Noise2DContext((int) System.currentTimeMillis());
		float[] pixels = new float[256 * 256];
		
		for (int i = 0; i < n; i++) {
			for (int y = 0; y < 256; y++) {
				for (int x = 0; x < 256; x++) {
					float v = n2d.get(x * 0.1f, y * 0.1f) * 0.5f + 0.5f;
					pixels[y * 256 + x] = v;
				}
			}
		}
		
		for (int y = 0; y < 256; y++) {
			for (int x = 0; x < 256; x++) {
				int idx = (int) (pixels[y * 256 + x] / 0.2f);
				System.out.print(symbols[idx]);
			}
			System.out.println();
		}
	}
	
	public static void main(String[] args) {
        int n = 1;
        if (args.length >= 1) n = Integer.parseInt(args[0]);
		new test(n);
	}
}
