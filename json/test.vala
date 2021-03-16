// valac --pkg posix --pkg json-glib-1.0 -D GCC_TEST test.vala

namespace Test
{
    class Program
    {
        public class Coordinate: GLib.Object
        {
            public double x { get; set; }
            public double y { get; set; }
            public double z { get; set; }

            public Coordinate() {}

            public Coordinate.xyz(double x, double y, double z)
            {
                this.x = x;
                this.y = y;
                this.z = z;
            }

            public bool equal (Coordinate? p)
            {
                if (p == null)
                {
                    return false;
                }

                if (this.get_class() != p.get_class())
                {
                    return false;
                }

                return ((this.x == p.x) && (this.y == p.y) && (this.z == p.z));
            }

            public string to_string()
            {
                return @"Coordinate(x=$(x), y=$(y), z=$(z))";
            }
        }

        static Coordinate Calc(string text)
        {
            Json.Node root = null;

            try
            {
                root = Json.from_string(text);
            }
            catch (Error e)
            {
                
            }

            var arr = root.get_object().get_array_member("coordinates");

            double x = 0;
            double y = 0;
            double z = 0;
            int count = (int)arr.get_length();

            foreach(var cnode in arr.get_elements())
            {   
                var c = (Coordinate)Json.gobject_deserialize(typeof(Coordinate),cnode);

                x += c.x;
                y += c.y;
                z += c.z;
            };

            return new Coordinate.xyz(x / count, y / count, z / count);
        }

        static void notify(string msg)
        {
            try
            {
                var socket = new SocketClient();
                var conn = socket.connect_to_host("localhost",9001,null);
                conn.output_stream.write (msg.data);
            }
            catch (Error e)
            {
                // standalone usage
            }
        }

        static void main(string[] args)
        {
            var right = new Coordinate.xyz(2.0, 0.5, 0.25);
            var string_list = new List<string>();
            string_list.append("{\"coordinates\": [{\"x\": 2.0, \"y\": 0.5, \"z\": 0.25}]}");
            string_list.append("{\"coordinates\": [{\"y\": 0.5, \"x\": 2.0, \"z\": 0.25}]}");

            foreach (var v in string_list) 
            {
                var left = Calc(v);
                if (!left.equal(right)) 
                {
                    stderr.printf(@"$(left) != $(right)");
                    Process.exit(1);
                }
            }

            string text;

            try
            {
                GLib.FileUtils.get_contents("/tmp/1.json", out text);
            }
            catch (Error e)
            {

            }
            
            var msg = "Vala/";
            #if GCC_TEST
                msg += "gcc";
            #elif CLANG_TEST
                msg += "clang";
            #else
                // The preprocessor directive for the test should be specified
                Process.exit(-1);
            #endif
            msg += @"\t $((uint16)Posix.getpid())";
            
            notify(msg);
            var results = Calc(text);
            notify("stop");
            
            stdout.printf(results.to_string()+"\n");
        }
    }
}
