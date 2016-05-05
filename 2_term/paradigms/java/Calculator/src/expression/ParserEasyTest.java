package expression;

/**
 * @author Georgiy Korneev
 */
public class ParserEasyTest extends ParserTest {
    protected ParserEasyTest() {
        levels.add(0, ops(
                new Op<>("<<", (a, b) -> a << b),
                new Op<>(">>", (a, b) -> a >> b)
        ));

        levels.get(2).add(new Op<>(" mod ", (a, b) -> b == 0 ? null : a % b));

        tests.addAll(ops(
                new Op<>("1 + 5 mod 3", (x, y, z) -> 3),
                new Op<>("x + y mod (z + 1)", (x, y, z) -> x + y % (z + 1)),
                new Op<>("1 << 5 + 3", (x, y, z) -> 256),
                new Op<>("x + y << z", (x, y, z) -> x + y << z),
                new Op<>("x * y << z", (x, y, z) -> x * y << z),
                new Op<>("x << y << z", (x, y, z) -> x << y << z),
                new Op<>("1024 >> 5 + 3", (x, y, z) -> 4),
                new Op<>("x + y >> z", (x, y, z) -> x + y >> z),
                new Op<>("x * y >> z", (x, y, z) -> x * y >> z),
                new Op<>("x >> y >> z", (x, y, z) -> x >> y >> z)
        ));
    }

    public static void main(final String[] args) {
        Util.checkAssert(ParserEasyTest.class);
        new ParserEasyTest().test();
    }
}
