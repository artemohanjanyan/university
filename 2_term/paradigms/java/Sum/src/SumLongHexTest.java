import java.util.ArrayList;
import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class SumLongHexTest {
    private static final SumChecker checker = new SumChecker("SumLongHex");

    public static void main(final String[] args) {
        checker.test(1, "1");
        checker.test(6, "1", "2", "3");
        checker.test(1, " 1");
        checker.test(1, "1 ");
        checker.test(1, "\t1\t");
        checker.test(12345, "\t12345\t");
        checker.test(1368, " 123 456 789 ");
        checker.test(60, "010", "020", "030");

        checker.test(1, "0x1");
        checker.test(0x1a, "0x1a");
        checker.test(0xA2, "0xA2");
        checker.test(62, " 0X0 0X1 0XF 0XF 0x0 0x1 0xF 0xf");
        checker.test(0x1234567890abcdefL, "0x1234567890abcdef");
        checker.test(0xCafeBabeDeadBeefL, "0xCafeBabeDeadBeef");

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
        randomTest(10, Long.MAX_VALUE);
        randomTest(100, Long.MAX_VALUE);
        checker.printStatus();
    }

    private static void randomTest(final int numbers, final long max) {
        long sum = 0;
        final List<String> args = new ArrayList<>();
        for (int i = 0; i < numbers; i++) {
            final long v = checker.random.nextLong() % max;
            args.add(checker.random.nextBoolean() ? String.valueOf(v) : "0X" + Long.toHexString(v));
            sum += v;
        }
        checker.testRandom(sum, args);
    }
}