import java.util.Arrays;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BaseBinarySearchTest {
    public static void test(final String className, final Solver solver) {
        final MainChecker checker = new MainChecker(className);

        for (final int size : new int[]{5, 4, 2, 1, 0, 10, 100}) {
            final int[] a = new int[size];
            for (final int max : new int[]{5, 4, 2, 1, 0, 10, 100, Integer.MAX_VALUE / 2}) {
                for (int i = 0; i < size; i++) {
                    a[i] = checker.random.nextInt(max * 2 + 1) - max;
                }
                Arrays.sort(a);
                for (int i = 0; i < size / 2; i++) {
                    final int t = a[i];
                    a[i] = a[size - i - 1];
                    a[size - i - 1] = t;
                }
                for (int i = 0; i < size; i++) {
                    test(checker, solver, a[i], a);
                    if (i != 0) {
                        test(checker, solver, (a[i - 1] + a[i]) / 2, a);
                    }
                }
                test(checker, solver, Integer.MIN_VALUE, a);
                test(checker, solver, Integer.MAX_VALUE, a);
            }
        }
        checker.printStatus();
    }

    private static void test(final MainChecker checker, final Solver solver, final int x, final int[] a) {
        final String[] as = new String[a.length + 1];
        as[0] = Integer.toString(x);
        for (int i = 0; i < a.length; i++) {
            as[i + 1] = Integer.toString(a[i]);
        }
        checker.test(solver.solve(a, x), as);
    }


    interface Solver {
        public long[] solve(final int[] a, final int x);
    }
}