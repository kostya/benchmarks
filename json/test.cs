using System;
using System.IO;
using System.Linq;
using Newtonsoft.Json.Linq;

namespace Test
{
    class Program
    {
        static void Main(string[] args)
        {
            var text = File.ReadAllText("./1.json");

            var root = JObject.Parse(text);
            var coordinates = (JArray)root["coordinates"];

            double x = 0;
            double y = 0;
            double z = 0;

            foreach(var c in coordinates)
            {
                x += (double)c["x"];
                y += (double)c["y"];
                z += (double)c["z"];
            };

            Console.WriteLine("{0}\n{1}\n{2}", x/coordinates.Count, y/coordinates.Count, z/coordinates.Count);
        }
    }
}
