using System;
using System.Diagnostics;
using System.Text;

namespace Test
{
    class Program
    {
        private static void Notify(string msg) {
            try {
                using (var s = new System.Net.Sockets.TcpClient("localhost", 9001)) {
                    var data = System.Text.Encoding.UTF8.GetBytes(msg);
                    s.Client.Send(data);
                }
            } catch {
                // standalone usage
            }
        }

        static void Main(string[] args)
        {
            foreach (var fixture in new string [][] {
                    new []{"hello", "aGVsbG8="}, new []{"world", "d29ybGQ="}}) {
                var src = fixture[0];
                var dst = fixture[1];
                var encoded =
                    Convert.ToBase64String(Encoding.UTF8.GetBytes(src));
                if (encoded != dst) {
                    Console.Error.WriteLine($"{encoded} != {dst}");
                    System.Environment.Exit(1);
                }
                var decoded =
                    Encoding.UTF8.GetString(Convert.FromBase64String(dst));
                if (decoded != src) {
                    Console.Error.WriteLine($"{decoded} != {src}");
                    System.Environment.Exit(1);
                }
            }

            const int STR_SIZE = 131072;
            const int TRIES = 8192;

            var str1 = Encoding.UTF8.GetBytes(new String('a', STR_SIZE));
            var str2 = Convert.ToBase64String(str1);
            var str3 = Convert.FromBase64String(str2);

            var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
            Notify($"C#/{runtime}\t{Process.GetCurrentProcess().Id}");

            var sw = Stopwatch.StartNew();
            var s_encoded = 0;
            for (var i = 0; i < TRIES; i++)
            {
                s_encoded += Convert.ToBase64String(str1).Length;
            }
            sw.Stop();
            var t_encoded = sw.Elapsed.TotalSeconds;

            var s_decoded = 0;
            sw.Restart();
            for (var i = 0; i < TRIES; i++)
            {
                s_decoded += Convert.FromBase64String(str2).Length;
            }
            sw.Stop();
            var t_decoded = sw.Elapsed.TotalSeconds;

            Notify("stop");

            Console.WriteLine("encode {0}... {1}...: {2}, {3}",
                              Encoding.UTF8.GetString(str1, 0, 4),
                              str2.Substring(0, 4),
                              s_encoded, t_encoded);
            Console.WriteLine("decode {0}... {1}...: {2}, {3}",
                              str2.Substring(0, 4),
                              Encoding.UTF8.GetString(str3, 0, 4),
                              s_decoded, t_decoded);
            Console.WriteLine("overall time: {0}s", t_encoded + t_decoded);
        }
    }
}
