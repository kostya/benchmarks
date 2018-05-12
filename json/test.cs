using System;
using System.IO;
using System.Collections.Generic;
using Utf8Json.Resolvers;
using Utf8Json;

class Root
{
    public struct Coordinate
    {
        public double X;
        public double Y;
        public double Z;
    }

    public List<Coordinate> Coordinates { get; set; }

#if NETCOREAPP2_0
    static async System.Threading.Tasks.Task Main(string[] args)
    {
        var stream = File.Open("./1.json", FileMode.Open);
        var root = await JsonSerializer.DeserializeAsync<Root>(stream, StandardResolver.AllowPrivateCamelCase);
#else
    static void Main(string[] args) {
        var text = File.ReadAllText("./1.json");
        var root = JsonSerializer.Deserialize<Root>(text, StandardResolver.AllowPrivateCamelCase);
#endif
        double x = 0;
        double y = 0;
        double z = 0;

        foreach (var c in root.Coordinates)
        {
            x += c.X;
            y += c.Y;
            z += c.Z;
        };

        Console.WriteLine(x / root.Coordinates.Count);
        Console.WriteLine(y / root.Coordinates.Count);
        Console.WriteLine(z / root.Coordinates.Count);
    }
}
