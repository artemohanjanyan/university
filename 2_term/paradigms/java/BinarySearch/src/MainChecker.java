import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Random;
import java.util.Scanner;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class MainChecker {
    private int total = 0;
    protected int passed = 0;
    private final Method method;
    public final Random random = new Random(8045702385702345702L);

    public MainChecker(final String className) {
        try {
            final URL url = new File(".").toURI().toURL();
            method = new URLClassLoader(new URL[]{url}).loadClass(className).getMethod("main", String[].class);
        } catch (final Exception e) {
            throw new AssertionError("Could not found main(String[]) in class "  + className, e);
        }
    }

    public String run(final String... input) {
        total++;
        System.err.format("Running test %02d: java %s \"%s\"\n", total, method.getDeclaringClass().getName(), join(input));
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        try {
            method.invoke(null, new Object[]{input});
            return out.toString("UTF-8");
        } catch (final InvocationTargetException e) {
            final Throwable cause = e.getCause();
            System.err.format("Error: %s %s\n", cause.getClass().getSimpleName(), cause.getMessage());
            cause.printStackTrace();
            return null;
        } catch (final Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    private String join(final String[] input) {
        final StringBuilder sb = new StringBuilder();
        for (final String s : input) {
            if (sb.length() > 0) {
                sb.append("\" \"");
            }
            sb.append(s);
        }
        return sb.toString();
    }

    private String join(final long[] values) {
        final StringBuilder sb = new StringBuilder();
        for (final long value : values) {
            if (sb.length() > 0) {
                sb.append(" ");
            }
            sb.append(value);
        }
        return sb.toString();
    }

    public void test(final long result, final String... input) {
        test(new long[]{result}, input);
    }

    public void test(final long[] result, final String... input) {
        final String output = run(input);
        if (output != null) {
            if (check(result, output)) {
                passed++;
            } else {
                System.err.println(String.format("Expected %s found %s", join(result), output));
            }
        }
    }

    protected boolean check(final long[] expected, final String output) {
        try {
            final Scanner scanner = new Scanner(output);
            for (final long e : expected) {
                if (scanner.nextLong() != e) {
                    return false;
                }
            }
            return true;
        } catch (final Exception e) {
            return false;
        }
    }

    public void printStatus() {
        System.err.println("===========================================");
        System.err.println(String.format("Test run: %d, passed; %d, failed: %d", total, passed, total - passed));
    }
}