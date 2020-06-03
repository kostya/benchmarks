package bench;

import com.dslplatform.json.*;
import java.io.*;
import java.util.*;
import java.nio.file.Files;
import java.nio.file.Paths;

public class TestJava {

    @CompiledJson
    public static class Root {
        public List<Model> coordinates;
    }

    public static class Model {
        public double x, y, z;
    }

    public static void parse(byte[] bytes) throws IOException {
        long start_time = System.currentTimeMillis();
        DslJson<Object> json = new DslJson<Object>(new DslJson.Settings<Object>().includeServiceLoader().doublePrecision(JsonReader.DoublePrecision.LOW));
        Root result = json.deserialize(Root.class, bytes, bytes.length);
        double x = 0, y = 0, z = 0;
        int total = result.coordinates.size();
        for (Model m : result.coordinates) {
            x += m.x;
            y += m.y;
            z += m.z;
        }
        System.out.println(x / total);
        System.out.println(y / total);
        System.out.println(z / total);
        System.out.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");
    }

    private static void notify(final String msg) {
        try (var socket = new java.net.Socket("localhost", 9001);
             var out = socket.getOutputStream()) {
            out.write(msg.getBytes("UTF-8"));
        } catch (java.io.IOException e) {
            // standalone usage
        }
    }

    public static void main(String[] args) throws IOException {
        var bytes = Files.readAllBytes(Paths.get("/tmp/1.json"));

        notify("Java\t" + ProcessHandle.current().pid());

        parse(bytes);

        notify("stop");
    }
}
