import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public final class Primes {
    private static final int UPPER_BOUND = 5000000;
    private static final int PREFIX = 32338;

    static final class Node {
        public final Map<Character, Node> children = new HashMap<>();
        public boolean terminal = false;
    }

    static final class Sieve {
        private final int limit;
        private final BitSet prime;

        Sieve(final int limit) {
            this.limit = limit;
            this.prime = new BitSet(limit + 1);
        }

        Iterable<Integer> toList() {
            final var result = new ArrayList<Integer>(Arrays.asList(2, 3));
            for (var p = 5; p <= this.limit; ++p) {
                if (this.prime.get(p)) {
                    result.add(p);
                }
            }
            return result;
        }

        Sieve omitSquares() {
            for (var r = 5; r * r < this.limit; ++r) {
                if (this.prime.get(r)) {
                    for (var i = r * r; i < this.limit; i += r * r) {
                        this.prime.clear(i);
                    }
                }
            }
            return this;
        }

        void step1(int x, int y) {
            final var n = (4 * x * x) + (y * y);
            if (n <= this.limit && (n % 12 == 1 || n % 12 == 5)) {
                this.prime.flip(n);
            }
        }

        void step2(int x, int y) {
            final var n = (3 * x * x) + (y * y);
            if (n <= this.limit && n % 12 == 7) {
                this.prime.flip(n);
            }
        }

        void step3(int x, int y) {
            final var n = (3 * x * x) - (y * y);
            if (x > y && n <= this.limit && n % 12 == 11) {
                this.prime.flip(n);
            }
        }

        void loopY(int x) {
            for (var y = 1; y * y < limit; ++y) {
                this.step1(x, y);
                this.step2(x, y);
                this.step3(x, y);
            }
        }

        void loopX() {
            for (var x = 1; x * x < limit; ++x) {
                this.loopY(x);
            }
        }

        Sieve calc() {
            this.loopX();
            return this.omitSquares();
        }
    }

    private static Node generateTrie(final Iterable<Integer> l) {
        final var root = new Node();
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
        final var primes = new Sieve(upperBound).calc();
        final var strPrefix = String.valueOf(prefix);
        var head = generateTrie(primes.toList());
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
        Collections.sort(result);
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
