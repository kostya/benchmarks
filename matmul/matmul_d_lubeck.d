#!/usr/bin/env dub
/+dub.sdl:
dependency "lubeck" version="~>1.3.0"
libs "lapack" "blas"
targetPath "target"
+/

import std.conv, std.array, std.stdio;
import mir.ndslice;
import kaleidic.lubeck : mtimes;
import std.socket;
import std.compiler;
import std.format;
import std.math;
import core.stdc.stdlib;
import core.thread;

alias Matrix = Slice!(double*, 2);

Matrix buildMatrix(in size_t n, in double seed)
{
    auto len = n * n;
    auto tmp = seed / len;
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

Matrix mul(Matrix a, Matrix b)
{
    return mtimes(a, b);
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
    auto a = buildMatrix(size, 1.0);
    auto b = buildMatrix(size, 2.0);
    auto x = mul(a, b);
    return x[size / 2, size / 2];
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

    notify("LDC lubeck\t%d".format(getpid()));

    printf("%.6f\n", calc(n));

    notify("stop");
}
