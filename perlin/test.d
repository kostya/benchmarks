import std.stdio;
import std.random;
import std.math;
import std.conv;

struct Vec2 {
	float x, y;
}

alias floor = core.stdc.math.floor;

static float lerp(immutable float a, immutable float b, immutable float v) pure nothrow
{
	return a * (1 - v) + b * v;
}

static float smooth(immutable float v) pure nothrow
{
	return v * v * (3 - 2 * v);
}

static Vec2 random_gradient(Random)(ref Random r)
{
	immutable v = uniform(0.0f, cast(float)PI * 2.0f, r);
	return Vec2(cos(v), sin(v));
}

static float gradient(immutable Vec2 orig, immutable Vec2 grad, immutable Vec2 p) pure nothrow
{
	immutable sp = Vec2(p.x - orig.x, p.y - orig.y);
	return grad.x * sp.x + grad.y * sp.y;
}

struct Noise2DContext {	
	Vec2[256] rgradients;
	uint[256] permutations;

private:
	Vec2 get_gradient(immutable int x, immutable int y) pure nothrow
	{
		immutable idx = permutations[x & 255] + permutations[y & 255];
		return rgradients[idx & 255];
	}

	Vec2[8] get_gradients(immutable float x, immutable float y) nothrow
	{
		float x0f = floor(x);
		float y0f = floor(y);
		int x0 = cast(int)x;
		int y0 = cast(int)y;
		int x1 = x0 + 1;
		int y1 = y0 + 1;

		return cast(Vec2[8]) [get_gradient(x0, y0),
		get_gradient(x1, y0),
		get_gradient(x0, y1),
		get_gradient(x1, y1),
		Vec2(x0f + 0.0f, y0f + 0.0f),
		Vec2(x0f + 1.0f, y0f + 0.0f),
		Vec2(x0f + 0.0f, y0f + 1.0f),
		Vec2(x0f + 1.0f, y0f + 1.0f)];
	}

public:
	static Noise2DContext opCall(uint seed)
	{
		Noise2DContext ret;
		auto rnd = Random(seed);
		foreach (ref elem; ret.rgradients)
			elem = random_gradient(rnd);

		foreach (i; 0 .. ret.permutations.length) {
			uint j = uniform(0, cast(uint)i+1, rnd);
			ret.permutations[i] = ret.permutations[j];
			ret.permutations[j] = cast(uint)i;
		}

		return ret;
	}

	float get(immutable float x, immutable float y) nothrow
	{
		immutable p = Vec2(x, y);

		immutable vecs = get_gradients(x, y);
		immutable v0 = gradient(vecs[4], vecs[0], p);
		immutable v1 = gradient(vecs[5], vecs[1], p);
		immutable v2 = gradient(vecs[6], vecs[2], p);
		immutable v3 = gradient(vecs[7], vecs[3], p);

		immutable fx = smooth(x - vecs[4].x);
		immutable vx0 = lerp(v0, v1, fx);
		immutable vx1 = lerp(v2, v3, fx);
		immutable fy = smooth(y - vecs[4].y);
		return lerp(vx0, vx1, fy);
	}
}

void main(string[] args)
{
	immutable symbols = [" ", "░", "▒", "▓", "█", "█"];
	auto pixels = new float[256*256];

    int n = args.length > 1 ? to!int(args[1]) : 1;

	auto n2d = Noise2DContext(0);
	foreach (immutable i; 0..n) {
		foreach (immutable y; 0..256) {
			foreach (immutable x; 0..256) {
				immutable v = n2d.get(x * 0.1f, y * 0.1f) *
					0.5f + 0.5f;
				pixels[y*256+x] = v;
			}
		}
	}

	foreach (immutable y; 0..256) {
		foreach (immutable x; 0..256) {
			write(symbols[cast(int)(pixels[y*256+x] / 0.2f)]);
		}
		writeln();
	}
}
