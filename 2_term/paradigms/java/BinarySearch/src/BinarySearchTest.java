/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BinarySearchTest extends BaseBinarySearchTest {
    public static void main(final String[] args) {
        test("BinarySearch", new Solver() {
            @Override
            public long[] solve(final int[] a, final int x) {
                for (int i = 0; i < a.length; i++) {
                    if (a[i] <= x) {
                        return new long[]{i};
                    }
                }
                return new long[]{a.length};
            }
        });
    }
}