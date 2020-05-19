#!/usr/bin/env dub
/+dub.sdl:
dependency "lubeck" version="~>1.3.0"
libs "lapack" "blas"
+/

import core.stdc.stdio, std.conv, std.array;
import mir.ndslice;
import kaleidic.lubeck: mtimes;
import std.socket;
import std.compiler;
import std.format;
import std.math;
import core.stdc.stdlib;
import core.thread;

alias Matrix = Slice!(double*, 2);

Matrix buildMatrix(size_t n)
{
	auto len = n * n;
	double tmp = 1.0 / len;
	auto a = slice!double(n, n);
	size_t i;
	foreach (ref row; a)
	{
		sizediff_t u = i, v = i;
		foreach (ref x; row)
		{
			x = tmp * (u * v);
			u--;
			v++;
		}
		i++;
	}
	return a;
}

Matrix[2] generate2(size_t n)
{
	auto a = buildMatrix(n);
	auto b = buildMatrix(n);
	return [a, b];
}

Matrix mul(Matrix a, Matrix b)
{
	return a.mtimes(b);
}

void notify(string msg) {
    try {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope(exit) socket.close();
        socket.send(msg);
    } catch (SocketOSException) {
        // standalone usage
    }
}

void main(in string[] args)
{
    size_t n = 100;
    if (args.length >= 2)
       n = to!size_t(args[1]) / 2 * 2;

    auto t = mul(buildMatrix(100), buildMatrix(100));
    if (abs(t[1][1] + 19.5) > 0.5) {
      exit(-1);
    }

    notify("LDC lubeck\t%d".format(getpid()));

    auto ab = generate2(n);
    auto x = mul(ab[0], ab[1]);
    printf("%.6f\n", x[n / 2, n / 2]);

    notify("stop");
}
