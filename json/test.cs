using System;
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

        static void Main(string[] args)
        {
            var text = File.ReadAllText("./1.json");

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
        }
    }
}
