using System;
using System.Buffers;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net.Sockets;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

class Program
{
    static void Verify()
    {
        var text = @"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
            ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
        var p_left = new Printer { Quiet = true };
        new Executable(text).Run(ref p_left);
        var left = p_left.Checksum;

        var p_right = new Printer { Quiet = true };
        foreach (var c in "Hello World!\n")
        {
            p_right.Print(c);
        }
        var right = p_right.Checksum;

        if (left != right)
        {
            Console.Error.WriteLine($"{left} != {right}");
            Environment.Exit(1);
        }
    }

    static void Notify(string msg)
    {
        try
        {
            using var s = new TcpClient("localhost", 9001);
            var data = Encoding.UTF8.GetBytes(msg);
            s.Client.Send(data);
        }
        catch
        {
            // standalone usage
        }
    }

    static void Main(string[] args)
    {
        Verify();
        var text = File.ReadAllText(args[0]);
        var p = new Printer
        {
            Quiet = Environment.GetEnvironmentVariable("QUIET") != null
        };

        var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
        Notify($"C# (Staged)/{runtime}\t{Environment.ProcessId}");
        var stopWatch = Stopwatch.StartNew();
        new Executable(text).Run(ref p);
        stopWatch.Stop();
        var elapsed = stopWatch.ElapsedMilliseconds / 1e3;

        Notify("stop");
        Console.Error.WriteLine($"time: {elapsed}s");

        if (p.Quiet)
        {
            Console.WriteLine($"Output checksum: {p.Checksum}");
        }
    }
}

readonly ref struct Executable
{
    private readonly ReadOnlySpan<char> _code;

    public Executable(ReadOnlySpan<char> code)
    {
        _code = code;
    }

    private delegate int Entrypoint(int address, scoped ref Span<byte> memory, ref Printer printer);

    private Type Emit()
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        static Type Throw(string? msg = null) => throw new InvalidProgramException(msg ?? "Unknown instruction");

        var cont = typeof(Stop);
        var nextAfterLoop = new Stack<Type>();
        for (int i = _code.Length - 1; i >= 0; i--)
        {
            var c = _code[i];
            switch (c)
            {
                case '>':
                    cont = typeof(AddPointer<>).MakeGenericType(cont);
                    break;
                case '<':
                    cont = typeof(SubPointer<>).MakeGenericType(cont);
                    break;
                case '+':
                    cont = typeof(AddData<>).MakeGenericType(cont);
                    break;
                case '-':
                    cont = typeof(SubData<>).MakeGenericType(cont);
                    break;
                case '.':
                    cont = typeof(OutputData<>).MakeGenericType(cont);
                    break;
                case ']':
                    nextAfterLoop.Push(cont);
                    cont = typeof(Stop);
                    break;
                case '[':
                    if (nextAfterLoop.Count == 0)
                    {
                        return Throw("Mismatched '[' without corresponding ']' ");
                    }
                    var next = nextAfterLoop.Pop();
                    var body = cont;
                    cont = typeof(Loop<,>).MakeGenericType(body, next);
                    break;
                default:
                    break;
            }
        }

        if (nextAfterLoop.Count != 0)
        {
            return Throw("Mismatched ']' without corresponding '[' ");
        }

        return cont;
    }

    public void Run(ref Printer printer)
    {
        var program = Emit();
        var entrypoint = program.GetMethod("Run")!.CreateDelegate<Entrypoint>();
        Span<byte> memory = new byte[1];
        entrypoint(0, ref memory, ref printer);
    }
}

interface IOp
{
    abstract static int Run(int address, scoped ref Span<byte> memory, ref Printer printer);
}

ref struct Stop : IOp
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        return address;
    }
}

ref struct Loop<Body, Next> : IOp
    where Body : IOp, allows ref struct
    where Next : IOp, allows ref struct
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        while (memory.AtNoRangeCheck(address) != 0)
        {
            address = Body.Run(address, ref memory, ref printer);
        }
        return Next.Run(address, ref memory, ref printer);
    }
}

ref struct AddPointer<Next> : IOp
    where Next : IOp, allows ref struct
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        if (address + 1 >= memory.Length)
        {
            memory.Enlarge();
        }
        return Next.Run(address + 1, ref memory, ref printer);
    }
}

ref struct AddData<Next> : IOp
    where Next : IOp, allows ref struct
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        memory.AtNoRangeCheck(address) += 1;
        return Next.Run(address, ref memory, ref printer);
    }
}

ref struct SubPointer<Next> : IOp
    where Next : IOp, allows ref struct
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        return Next.Run(address - 1, ref memory, ref printer);
    }
}

ref struct SubData<Next> : IOp
    where Next : IOp, allows ref struct
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        memory.AtNoRangeCheck(address) -= 1;
        return Next.Run(address, ref memory, ref printer);
    }
}

ref struct OutputData<Next> : IOp
    where Next : IOp, allows ref struct
{
    public static int Run(int address, scoped ref Span<byte> memory, ref Printer printer)
    {
        printer.Print(memory.AtNoRangeCheck(address));
        return Next.Run(address, ref memory, ref printer);
    }
}

ref struct Printer
{
    int sum1, sum2;
    public readonly int Checksum
    {
        get => (sum2 << 8) | sum1;
    }

    public bool Quiet { get; set; }

    public void Print(int n)
    {
        if (Quiet)
        {
            sum1 = (sum1 + n) % 255;
            sum2 = (sum2 + sum1) % 255;
        }
        else
        {
            Console.Write((char)n);
        }
    }
}

static class MemoryExtensions
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref byte AtNoRangeCheck(this ref Span<byte> memory, int address)
    {
        // It's safe because the address has been guarded in AddPointer.
        return ref Unsafe.Add(ref MemoryMarshal.GetReference(memory), address);
    }
    
    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static void Enlarge(this ref Span<byte> memory)
    {
        var newMemory = new Span<byte>(new byte[memory.Length * 2]);
        memory.CopyTo(newMemory);
        memory = newMemory;
    }
}