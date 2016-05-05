import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;
import java.util.ListIterator;
import java.util.Random;
import java.util.Scanner;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 * @version $$Id$$
 */
public class SumChecker {
    private int total = 0;
    private int passed = 0;
    private final Method method;
    public final Random random = new Random(8045702385702345702L);

    public SumChecker(final String className) {
        try {
            final URL url = new File(".").toURI().toURL();
            method = new URLClassLoader(new URL[]{url}).loadClass(className).getMethod("main", String[].class);
        } catch (final Exception e) {
            throw new AssertionError("Could not found main(String[]) in class "  + className, e);
        }
    }

    public void test(final long result, final String... input) {
        total++;
        System.err.format("Running test %02d: java %s \"%s\"\n", total, method.getDeclaringClass().getName(), String.join("\" \"", input));
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        try {
            method.invoke(null, new Object[]{input});
            final String output = out.toString("UTF-8");
            if (!check(result, output)) {
                System.err.format("Expected %d, found %s\n", result, output);
                return;
            }
        } catch (final InvocationTargetException e) {
            final Throwable cause = e.getCause();
            System.err.format("Error: %s %s\n", cause.getClass().getSimpleName(), cause.getMessage());
            cause.printStackTrace();
            return;
        } catch (final Exception e) {
            e.printStackTrace();
            return;
        }
        passed++;
    }

    private static boolean check(final long expected, final String output) {
        try {
            return new Scanner(output).nextLong() == expected;
        } catch (final Exception e) {
            return false;
        }
    }

    public void printStatus() {
        System.err.println(String.format("Test run: %d, passed; %d, failed: %d", total, passed, total - passed));
    }

    public void testRandom(final long result, final List<String> args) {
        for (final ListIterator<String> li = args.listIterator(); li.hasNext(); ) {
            li.set(randomSpace() + li.next() + randomSpace());
        }
        for (final ListIterator<String> li = args.listIterator(); li.hasNext(); ) {
            String next = li.next();
            if (li.hasNext() && random.nextBoolean()) {
                next += " " + randomSpace() + li.next();
                li.remove();
                li.previous();
                li.set(next);
            }
        }
        test(result, args.toArray(new String[args.size()]));
    }

    private String randomSpace() {
        return random.nextBoolean() ? "" : " \t\n\u000B\f".charAt(random.nextInt(5)) + randomSpace();
    }
}
