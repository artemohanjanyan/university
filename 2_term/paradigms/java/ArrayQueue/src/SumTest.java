import java.util.ArrayList;
import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class SumTest {
    private static final SumChecker checker = new SumChecker("Sum");

    public static void main(final String[] args) {
        checker.test(1, "1");
        checker.test(6, "1", "2", "3");
        checker.test(1, " 1");
        checker.test(1, "1 ");
        checker.test(1, "\t1\t");
        checker.test(12345, "\t12345\t");
        checker.test(1368, " 123 456 789 ");
        checker.test(-1, "-1");
        checker.test(-6, "-1", "-2", "-3");
        checker.test(-12345, "\t-12345\t");
        checker.test(-1368, " -123 -456 -789 ");
        checker.test(1, "+1");
        checker.test(6, "+1", "+2", "+3");
        checker.test(12345, "\t+12345\t");
        checker.test(1368, " +123 +456 +789 ");
        checker.test(0);
        checker.test(0, " ");
        randomTest(10, 100);
        randomTest(10, Integer.MAX_VALUE);
        randomTest(100, Integer.MAX_VALUE);
        checker.printStatus();
    }

    private static void randomTest(final int numbers, final int max) {
        int sum = 0;
        final List<String> args = new ArrayList<>();
        for (int i = 0; i < numbers; i++) {
            final int v = checker.random.nextInt() % max;
            args.add(String.valueOf(v));
            sum += v;
        }
        checker.testRandom(sum, args);
    }
}
