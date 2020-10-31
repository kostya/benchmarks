using System;
using System.Diagnostics;
using System.IO;
using System.Text.Json;
using System.Collections.Generic;

namespace Test
{
    class Program
    {
        public class Coordinate: IEquatable<Coordinate>
        {
            public double X { get; set; }
            public double Y { get; set; }
            public double Z { get; set; }

            public Coordinate() {}

            public Coordinate(double x, double y, double z)
            {
                X = x;
                Y = y;
                Z = z;
            }

            public override bool Equals(object obj)
            {
                return this.Equals(obj as Coordinate);
            }

            public bool Equals(Coordinate p)
            {
                if (Object.ReferenceEquals(p, null))
                {
                    return false;
                }

                if (Object.ReferenceEquals(this, p))
                {
                    return true;
                }

                if (this.GetType() != p.GetType())
                {
                    return false;
                }

                return X == p.X && Y == p.Y && Z == p.Z;
            }

            public override int GetHashCode()
            {
                return HashCode.Combine(X, Y, Z);
            }

            public static bool operator ==(Coordinate lhs, Coordinate rhs)
            {
                if (Object.ReferenceEquals(lhs, null))
                {
                    if (Object.ReferenceEquals(rhs, null))
                    {
                        return true;
                    }
                    return false;
                }
                return lhs.Equals(rhs);
            }

            public static bool operator !=(Coordinate lhs, Coordinate rhs)
            {
                return !(lhs == rhs);
            }

            public override string ToString()
            {
                return $"Coordinate {{X: {X}, Y: {Y}, Z: {Z}}}";
            }
        }

        public class Root
        {
            public List<Coordinate> Coordinates { get; set; }
        }

        static Coordinate Calc(string text)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
            };
            var root = JsonSerializer.Deserialize<Root>(text, options);

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

            return new Coordinate(x / count, y / count, z / count);
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
            var right = new Coordinate(2.0, 0.5, 0.25);
            foreach (var v in new List<string> {
                    "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
                    "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"
                }) {
                var left = Calc(v);
                if (left != right) {
                    Console.Error.WriteLine($"{left} != {right}");
                    System.Environment.Exit(1);
                }
            }

            var text = File.ReadAllText("/tmp/1.json");

            Notify($"C#/.NET Core (System.Text.Json)\t{Process.GetCurrentProcess().Id}");
            var results = Calc(text);
            Notify("stop");

            Console.WriteLine(results);
        }
    }
}
