//valac --pkg gio-2.0 --pkg posix -D GCC_TEST test.vala

namespace Test
{
    class Program
    {
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

		public static void main(string[] args)
		{
			string[,] fixtures = 
			{{"hello", "aGVsbG8="}, {"world", "d29ybGQ="}};
			
			var num_fixtures = fixtures.length[0];

			for (var i=0;i<num_fixtures;i++)
			{	
				var src = fixtures[i,0];
				var dst = fixtures[i,1];
				var encoded = Base64.encode(src.data);

				if (encoded!=dst)
				{
					stderr.printf("%s != %s", encoded, dst);
					Process.exit(1);
				}

				var decoded = (string) Base64.decode(dst);

				if (decoded!=src)
				{
					stderr.printf("%s != %s", decoded, src);
					Process.exit(1);
				}
			}

			var STR_SIZE = 131072;
			var TRIES = 8192;
		
			var str1 = string.nfill(STR_SIZE,'a');
			var str2 = Base64.encode(str1.data);
			var str3 = (string) Base64.decode(str2);

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

			var s_encoded = 0;
            var timer = new Timer();
			for (var i = 0; i < TRIES; i++)
			{
				s_encoded += Base64.encode(str1.data).length;
			}
            timer.stop();

			var t_encoded = timer.elapsed();

			var s_decoded = 0;
            timer.reset();
			for (var i = 0; i < TRIES; i++)
			{
				s_decoded += ((string) Base64.decode(str2)).length;
			}
            timer.stop();

			var t_decoded = timer.elapsed();
			notify("stop");
			
			stdout.printf("encode %.4s... to %.4s...: %d, %.2f\n",
				str1, str2, s_encoded, t_encoded);
		 	stdout.printf("decode %.4s... to %.4s...: %d, %.2f\n",
				str2, str3, s_decoded, t_decoded);
			
		}
	}
}
