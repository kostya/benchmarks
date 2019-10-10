using System;
using System.Diagnostics;
using System.Text;

namespace Test
{
    class Program
    {
        static void Main(string[] args)
        {
            const int STR_SIZE = 10000000;
            const int TRIES = 100;

            byte[] str1 = Encoding.ASCII.GetBytes(new String('a', STR_SIZE));
            string str2 = String.Empty;
            int s = 0;

            Console.WriteLine("JIT warming up");
            for (int i = 0; i < 5; i++)
            {
                Convert.FromBase64String(Convert.ToBase64String(str1));
            }

            Console.WriteLine("run");
            var sw = Stopwatch.StartNew();
            for (int i = 0; i < TRIES; i++)
            {
                str2 = Convert.ToBase64String(str1);
                s += str2.Length;
            }

            sw.Stop();
            Console.WriteLine("encode: {0}, {1}", s, sw.Elapsed.TotalSeconds);
            var overall = sw.Elapsed.TotalSeconds;

            s = 0;
            sw.Restart();
            for (int i = 0; i < TRIES; i++)
            {
                s += Convert.FromBase64String(str2).Length;
            }
            sw.Stop();
            Console.WriteLine("decode: {0}, {1}", s, sw.Elapsed.TotalSeconds);
            overall += sw.Elapsed.TotalSeconds;
            Console.WriteLine("overall time: {0}s", overall);
        }
    }
}
