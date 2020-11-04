// Originally written by Attractive Chaos; distributed under the MIT license (D V.2 code)
// Contributed by leonardo and then modified by Attractive Chaos to remove D 2.0 features

import std.numeric, std.stdio, std.string, std.conv;
import std.socket;
import std.compiler;
import std.format;
import std.math;
import core.stdc.stdlib;
import core.thread;

double[][] matGen(in size_t n, in double seed)
{
    auto len = n * n;
    auto tmp = seed / len;
    auto a = new double[][](n, n);
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

double[][] matMul(in double[][] a, in double[][] b)
{
    auto m = a.length, n = a[0].length, p = b[0].length;

    // transpose
    auto c = new double[][](p, n);
    foreach (i, brow; b)
        foreach (j, bx; brow)
            c[j][i] = bx;

    auto x = new double[][](m, p);

    foreach (i, arow; a)
        foreach (j, crow; c)
            x[i][j] = dotProduct(arow, crow);

    return x;
}

void notify(in string msg)
{
    try
    {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope (exit)
            socket.close();
        socket.send(msg);
    }
    catch (SocketOSException)
    {
        // standalone usage
    }
}

double calc(in size_t n)
{
    auto size = n / 2 * 2;
    auto a = matGen(size, 1.0);
    auto b = matGen(size, 2.0);
    auto x = matMul(a, b);
    return x[size / 2][size / 2];
}

void main(in string[] args)
{
    auto n = args.length > 1 ? to!size_t(args[1]) : 100;

    auto left = calc(101);
    auto right = -18.67;
    if (abs(left - right) > 0.1)
    {
        stderr.writefln("%f != %f", left, right);
        exit(1);
    }

    notify("%s\t%d".format(name, getpid()));
    immutable results = calc(n);
    notify("stop");

    printf("%.6f\n", results);
}
