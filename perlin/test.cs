using System;

struct Vec2 {
	public float x;
	public float y;
}

class Noise2DContext {
	Vec2[] rgradients;
	int[] permutations;
	Vec2[] gradients;
	Vec2[] origins;

	internal static float lerp(float a, float b, float v) {
		return a * (1 - v) + b * v;
	}

	internal static float smooth(float v) {
		return v * v * (3 - 2 * v);
	}

	internal static Vec2 random_gradient(Random rnd) {
		var v = rnd.NextDouble() * Math.PI * 2.0;
		return new Vec2 { x = (float)Math.Cos(v), y = (float)Math.Sin(v) };
	}

	internal static float gradient(Vec2 orig, Vec2 grad, Vec2 p) {
		return grad.x * (p.x - orig.x) + grad.y * (p.y - orig.y);
	}

	public Noise2DContext(int seed) {
		Random rnd = new Random(seed);
		rgradients = new Vec2[256];
		permutations = new int[256];
		for (int i = 0; i < 256; i++) {
			rgradients[i] = random_gradient(rnd);
		}
		for (int i = 0; i < 256; i++) {
			int j = rnd.Next(i + 1);
			permutations[i] = permutations[j];
			permutations[j] = i;
		}

		gradients = new Vec2[4];
		origins = new Vec2[4];
	}

	internal Vec2 get_gradient(int x, int y) {
		int idx = permutations[x & 255] + permutations[y & 255];
		return rgradients[idx & 255];
	}

	internal void get_gradients(float x, float y) {
		float x0f = (float)Math.Floor(x);
		float y0f = (float)Math.Floor(y);
		int x0 = (int)x0f;
		int y0 = (int)y0f;
		int x1 = x0 + 1;
		int y1 = y0 + 1;

		gradients[0] = get_gradient(x0, y0);
		gradients[1] = get_gradient(x1, y0);
		gradients[2] = get_gradient(x0, y1);
		gradients[3] = get_gradient(x1, y1);

		origins[0].x = x0f + 0;
		origins[0].y = y0f + 0;
		origins[1].x = x0f + 1;
		origins[1].y = y0f + 0;
		origins[2].x = x0f + 0;
		origins[2].y = y0f + 1;
		origins[3].x = x0f + 1;
		origins[3].y = y0f + 1;
	}

	public float get(float x, float y) {
		Vec2 p = new Vec2 { x = x, y = y };
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

class Application {
	static readonly char[] symbols = { ' ', '░', '▒', '▓', '█', '█' };

	public static void Main(string[] args) {
		var n2d = new Noise2DContext((int)DateTime.Now.Ticks);
		float[] pixels = new float[256 * 256];

        var n = args.Length >= 1 ? int.Parse(args[0]) : 1;

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
				int idx = (int)(pixels[y * 256 + x] / 0.2f);
				Console.Write(symbols[idx]);
			}
			Console.WriteLine();
		}
	}
}
