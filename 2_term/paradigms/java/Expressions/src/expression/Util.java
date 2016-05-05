package expression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Util {
    // Utility class
    private Util() {}

    public static void assertTrue(final boolean condition, final String message) {
        assert condition : message;
    }

    public static void assertEquals(final String message, final int actual, final int expected) {
        assertTrue(actual == expected, String.format("%s: Expected %d, found %d", message, expected, actual));
    }

    public static void assertEquals(final String message, final double actual, final double expected) {
        assertTrue(Math.abs(actual - expected) < 1e-9, String.format("%s: Expected %f, found %f", message, expected, actual));
    }

    public static void checkAssert(final Class<?> c) {
        boolean assertsEnabled = false;
        assert assertsEnabled = true;
        if (!assertsEnabled) {
            throw new AssertionError("You should enable assertions by running 'java -ea " + c.getName() + "'");
        }
    }
}
