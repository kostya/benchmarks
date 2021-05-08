import java.util.Arrays;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.HashSet;
import java.util.Queue;
import java.util.List;
import java.util.AbstractMap.SimpleEntry;
import java.util.function.Function;

public final class Primes {
    private final static int UPPER_BOUND = 5000000;
    private final static int PREFIX = 32338;

    static final class Node {
        public final Map<Character, Node> children = new HashMap<>();
        public boolean terminal = false;
    }

    private static Iterable<Integer> generatePrimes(final int m) {
        final var result = new HashSet<Integer>(Arrays.asList(2));
        for (var i = 1; i < 1 + (m - 1) / 2; i++) {
            result.add(2 * i + 1);
        }
        var k = 1;
        var j = 1;
        final Function<Integer, Integer> sqr = (i) -> { return i * i; };
        final Function<Integer, Integer> maxN = (i) -> {
            return (m - sqr.apply(2 * i + 1)) / (4 * i + 2);
        };
        while (k > 0) {
            k = maxN.apply(j++);
        }
        k = j;
        for (var i = 1; i < k + 1; i++) {
            for (var n = 0; n < maxN.apply(i - 1); n++) {
                result.remove((2 * i + 1) * (2 * i + 2 * n + 1));
            }
        }
        return result;
    }

    private static Node generateTrie(final Iterable<Integer> l) {
        var root = new Node();
        for (final var el : l) {
            var head = root;
            for (final var ch : String.valueOf(el).toCharArray()) {
                if (!head.children.containsKey(ch)) {
                    head.children.put(ch, new Node());
                }
                head = head.children.get(ch);
            }
            head.terminal = true;
        }
        return root;
    }

    private static Iterable<Integer> find(int upperBound, int prefix) {
        final var primes = generatePrimes(upperBound);
        final var root = generateTrie(primes);
        final var strPrefix = String.valueOf(prefix);
        var head = root;
        for (final var ch : strPrefix.toCharArray()) {
            if (head.children.containsKey(ch)) {
                head = head.children.get(ch);
            } else {
                return null;
            }
        }
        final Queue<Map.Entry<Node, String>> queue = new LinkedList<>(
            Arrays.asList(new SimpleEntry<Node, String>(head, strPrefix)));
        final List<Integer> result = new ArrayList<>();
        while (queue.peek() != null) {
            final var queueTop = queue.poll();
            final var top = queueTop.getKey();
            final var currentPrefix = queueTop.getValue();
            if (top.terminal) {
                result.add(Integer.valueOf(currentPrefix));
            }
            for (final var entry : top.children.entrySet()) {
                queue.add(new SimpleEntry<Node, String>(
                              entry.getValue(),
                              currentPrefix + entry.getKey()
                          ));
            }
        }
        return result;
    }

    private static void notify(final String msg) {
        try (final var socket = new java.net.Socket("localhost", 9001);
             final var out = socket.getOutputStream()) {
            out.write(msg.getBytes("UTF-8"));
        } catch (final java.io.IOException e) {
            // standalone usage
        }
    }

    private static void verify() {
        final var left = Arrays.asList(2, 23, 29);
        final var right = find(100, 2);
        if (!left.toString().equals(right.toString())) {
            System.err.printf("%s != %s\n", left, right);
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        verify();

        notify("Java\t" + ProcessHandle.current().pid());
        final var results = find(UPPER_BOUND, PREFIX);
        notify("stop");

        System.out.println(results);
    }
}
