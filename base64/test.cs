using System;
using System.Diagnostics;
using System.Text;

namespace Test
{
    class Program
    {
        static void Main(string[] args)
        {
            const int STR_SIZE = 131072;
            const int TRIES = 8192;

            var str1 = Encoding.ASCII.GetBytes(new String('a', STR_SIZE));
            var s = 0;

            Console.WriteLine("JIT warming up");
            for (var i = 0; i < 5; i++)
            {
                Convert.FromBase64String(Convert.ToBase64String(str1));
            }

            var str2 = Convert.ToBase64String(str1);
            Console.Write("encode {0}... {1}...: ",
                Encoding.UTF8.GetString(str1, 0, 4), str2.Substring(0, 4));

            var sw = Stopwatch.StartNew();
            for (var i = 0; i < TRIES; i++)
            {
                str2 = Convert.ToBase64String(str1);
                s += str2.Length;
            }

            sw.Stop();
            Console.WriteLine("{0}, {1}", s, sw.Elapsed.TotalSeconds);
            var overall = sw.Elapsed.TotalSeconds;

            var str3 = Convert.FromBase64String(str2);
            Console.Write("decode {0}... {1}...: ",
                str2.Substring(0, 4), Encoding.UTF8.GetString(str3, 0, 4));

            s = 0;
            sw.Restart();
            for (var i = 0; i < TRIES; i++)
            {
                str3 = Convert.FromBase64String(str2);
                s += str3.Length;
            }
            sw.Stop();
            Console.WriteLine("{0}, {1}", s, sw.Elapsed.TotalSeconds);
            overall += sw.Elapsed.TotalSeconds;
            Console.WriteLine("overall time: {0}s", overall);
        }
    }
}
