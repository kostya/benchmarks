import java.util.Arrays;
import java.util.Base64;
import static java.lang.System.out;

class Base64Java {

    final static int STR_SIZE = 131072;
    final static int TRIES = 8192;

    final static Base64.Decoder dec = Base64.getDecoder();
    final static Base64.Encoder enc = Base64.getEncoder();

    private static void notify(final String msg) {
        try (final var socket = new java.net.Socket("localhost", 9001);
             final var out = socket.getOutputStream()) {
            out.write(msg.getBytes("UTF-8"));
        } catch (java.io.IOException e) {
            // standalone usage
        }
    }

    public static void main(String[] args){
        for (final var fixture: new String[][]{
                {"hello", "aGVsbG8="}, {"world", "d29ybGQ="}
            }) {
            final var src = fixture[0];
            final var dst = fixture[1];
            final var encoded = enc.encodeToString(src.getBytes());
            if (!encoded.equals(dst)) {
                System.err.printf("%s != %s\n", encoded, dst);
                System.exit(1);
            }
            final var decoded = new String(dec.decode(dst));
            if (!decoded.equals(src)) {
                System.err.printf("%s != %s\n", decoded, src);
                System.exit(1);
            }
        }

        final var str = new byte[STR_SIZE];
        Arrays.fill(str, (byte)'a');
        final var str2 = enc.encodeToString(str);
        final var encoded = str2.getBytes();
        final var b_arr = dec.decode(encoded);

        notify("Java\t" + ProcessHandle.current().pid());

        var s_encoded = 0;
        final var t = System.nanoTime();
        for (var i = 0 ; i < TRIES ; i++) {
            s_encoded += enc.encodeToString(str).length();
        }
        final var t_encoded = (System.nanoTime() - t) / 1e9;

        var s_decoded = 0;
        final var t1 = System.nanoTime();
        for (var i = 0 ; i < TRIES ; i++) {
            s_decoded += dec.decode(encoded).length;
        }
        final var t_decoded = (System.nanoTime() - t1) / 1e9;

        notify("stop");

        out.println(
            String.format(
                "encode %s... to %s...: %d, %.2f",
                new String(Arrays.copyOf(str, 4)),
                str2.substring(0, 4),
                s_encoded, t_encoded));

        out.println(
            String.format("decode %s... to %s...: %d, %.2f",
                          str2.substring(0, 4),
                          new String(Arrays.copyOf(b_arr, 4)),
                          s_decoded, t_decoded));
        out.println("overall time: " + (t_encoded + t_decoded) + "s");
    }
}
