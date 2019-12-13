import java.util.Arrays;
import java.util.Base64;
import static java.lang.System.out;

class Base64Java {

    final static int STR_SIZE = 131072;
    final static int TRIES = 8192;

    final static Base64.Decoder dec = Base64.getDecoder();
    final static Base64.Encoder enc = Base64.getEncoder();

    public static void main(String[] args){
        final byte[] str = new byte[STR_SIZE];
        Arrays.fill(str, (byte)'a');

        out.println("JIT warming up");
        for (int i = 0 ; i < 5 ; i++) {
            dec.decode(enc.encodeToString(str));
        }

        try (var socket = new java.net.Socket("localhost", 9001);
             var out = socket.getOutputStream()) {
            out.write("Java".getBytes("UTF-8"));
        } catch (java.io.IOException e) {
            // standalone usage
        }

        String str2 = enc.encodeToString(str);
        out.print(String.format("encode %s... to %s...: ",
            new String(Arrays.copyOf(str, 4)),
            str2.substring(0, 4)));

        int s = 0;
        final Long t = System.nanoTime();
        for (int i = 0 ; i < TRIES ; i++) {
            str2 = enc.encodeToString(str);
            s += str2.length();
        }
        out.println(String.format("%d, %.2f", s, (System.nanoTime() - t) / 1e9));

        final byte[] encoded = str2.getBytes();
        byte[] b_arr = dec.decode(encoded);
        out.print(String.format("decode %s... to %s...: ",
            str2.substring(0, 4),
            new String(Arrays.copyOf(b_arr, 4))));

        s = 0;
        final Long t1 = System.nanoTime();
        for (int i = 0 ; i < TRIES ; i++) {
            b_arr = dec.decode(encoded);
            s += b_arr.length;
        }
        final Long now = System.nanoTime();
        out.println(String.format("%d, %.2f", s, (now - t1) / 1e9));
        out.println("overall time: " + (now - t) / 1e9 + "s");
    }
}
