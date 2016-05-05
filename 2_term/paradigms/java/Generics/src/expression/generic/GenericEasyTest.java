package expression.generic;

import static expression.Util.checkAssert;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class GenericEasyTest extends GenericTest {
    public GenericEasyTest() {
        all(
                "1 + 5 mod 3",
                (x, y, z) -> 1 + 5 % 3,
                (x, y, z) -> 1 + 5 % 3.0,
                (x, y, z) -> bi(1 + 5 % 3)
        );
        all(
                "x + y mod (z + 1)",
                (x, y, z) -> x + y % (z + 1),
                (x, y, z) -> x + y % (z + 1.0),
                (x, y, z) -> bi(x + y % (z + 1))
        );
        all(
                "abs -5",
                (x, y, z) -> 5,
                (x, y, z) -> 5.0,
                (x, y, z) -> bi(5)
        );
        all(
                "abs (x - y) / z",
                (x, y, z) -> Math.abs(x - y) / z,
                (x, y, z) -> Math.abs(x - y) / (double) z,
                (x, y, z) -> bi(Math.abs(x - y) / z)
        );
        all(
                "square -5",
                (x, y, z) -> 25,
                (x, y, z) -> 25.0,
                (x, y, z) -> bi(25)
        );
        all(
                "square x - y / z",
                (x, y, z) -> x * x - y / z,
                (x, y, z) -> x * x - y / (double) z,
                (x, y, z) -> bi(x * x - y / z)
        );
    }

    public static void main(final String[] args) {
        checkAssert(GenericEasyTest.class);
        new GenericEasyTest().test();
    }
}
