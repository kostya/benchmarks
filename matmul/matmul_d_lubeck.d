#!/usr/bin/env dub
/+dub.sdl:
dependency "lubeck" version="~>1.1.7"
libs "lapack" "blas"
+/

import core.stdc.stdio, std.conv, std.array;
import mir.ndslice;
import lubeck: mtimes;
import std.socket;
import std.compiler;

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

void main(in string[] args)
{
    try {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope(exit) socket.close();
        socket.send("LDC lubeck");
    } catch (SocketOSException) {
        // standalone usage
    }

    size_t n = 100;
    if (args.length >= 2)
       n = to!size_t(args[1]) / 2 * 2;
    auto ab = generate2(n);
    auto x = mul(ab[0], ab[1]);
    printf("%.6f\n", x[n / 2, n / 2]);
}
