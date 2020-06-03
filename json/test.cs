using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Newtonsoft.Json.Linq;
using System.Collections.Generic;
using Newtonsoft.Json;

namespace Test
{
    class Program
    {
        public class Coordinate
        {
            public double X { get; set; }
            public double Y { get; set; }
            public double Z { get; set; }
        }

        public class Root
        {
            public List<Coordinate> Coordinates { get; set; }
        }

        static void ParseJson(string text)
        {
            var sw = Stopwatch.StartNew();

            var root = JsonConvert.DeserializeObject<Root>(text);

            double x = 0;
            double y = 0;
            double z = 0;
            int count = 0;

            foreach(var c in root.Coordinates)
            {
                count += 1;
                x += c.X;
                y += c.Y;
                z += c.Z;
            };

            Console.WriteLine("{0}\n{1}\n{2}", x/count, y/count, z/count);
            sw.Stop();
            Console.WriteLine("time: {0}s", sw.Elapsed.TotalSeconds);
        }

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
            var text = File.ReadAllText("/tmp/1.json");

            var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
            Notify($"C# {runtime}\t{Process.GetCurrentProcess().Id}");

            ParseJson(text);

            Notify("stop");}
    }
}
