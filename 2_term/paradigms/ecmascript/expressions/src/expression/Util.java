package expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Util {
    public static final Random RNG = new Random(58L);

    // Utility class
    private Util() {}

    public static void assertTrue(final String message, final boolean condition) {
        assert condition : message;
    }

    public static void assertEquals(final String message, final int actual, final int expected) {
        assertTrue(String.format("%s: Expected %d, found %d", message, expected, actual), actual == expected);
    }

    public static void assertEquals(final String message, final double actual, final double expected) {
        assertTrue(
                String.format("%s: Expected %f, found %f", message, expected, actual),
                Math.abs(actual - expected) < 1e-9 ||
                        Double.isNaN(actual) && Double.isNaN(expected) ||
                        Double.isInfinite(actual) && Double.isInfinite(expected) && Math.signum(actual) == Math.signum(expected)
        );
    }

    public static void checkAssert(final Class<?> c) {
        boolean assertsEnabled = false;
        assert assertsEnabled = true;
        if (!assertsEnabled) {
            throw new AssertionError("You should enable assertions by running 'java -ea " + c.getName() + "'");
        }
    }

    public static String repeat(final String s, final int n) {
        return Stream.generate(() -> s).limit(n).collect(Collectors.joining());
    }

    public static <T> T random(final List<T> variants) {
        return variants.get(RNG.nextInt(variants.size()));
    }

    @SafeVarargs
    public static <T> T random(final T... variants) {
        return random(Arrays.asList(variants));
    }

    public static int randomInt(final int n) {
        return RNG.nextInt(n);
    }

    @SafeVarargs
    public static <T> List<T> list(final T... items) {
        return new ArrayList<>(Arrays.asList(items));
    }

    public static void addRange(final List<Integer> values, final int d, final int c) {
        for (int i = -d; i <= d; i++) {
            values.add(c + i);
        }
    }

    public static final class Op<T> {
        public final String name;
        public final T f;

        private Op(final String name, final T f) {
            this.name = name;
            this.f = f;
        }
    }

    public static <T> Op<T> op(final String name, final T f) {
        return new Op<>(name, f);
    }
}
