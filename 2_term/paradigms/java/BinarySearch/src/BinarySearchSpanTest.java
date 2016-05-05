/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BinarySearchSpanTest extends BaseBinarySearchTest {
    public static void main(final String[] args) {
        test("BinarySearchSpan", new Solver() {
            @Override
            public long[] solve(final int[] a, final int x) {
                for (int i = 0; i < a.length; i++) {
                    if (a[i] == x) {
                        int j = i;
                        while (j < a.length && a[j] == x) {
                            j++;
                        }
                        return new long[]{i, j - i};
                    }
                    if (x > a[i]) {
                        return new long[]{i, 0};
                    }
                }
                return new long[]{a.length, 0};
            }
        });
    }
}