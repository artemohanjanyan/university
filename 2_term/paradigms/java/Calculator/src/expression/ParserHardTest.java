package expression;

/**
 * @author Georgiy Korneev
 */
public class ParserHardTest extends ParserEasyTest {
    protected ParserHardTest() {
        unary.add(new Op<>(" abs ", Math::abs));
        unary.add(new Op<>(" square ", a -> a * a));

        tests.addAll(ops(
                new Op<>("abs -5", (x, y, z) -> 5),
                new Op<>("abs (x - y)", (x, y, z) -> Math.abs(x - y)),
                new Op<>("abs -x", (x, y, z) -> Math.abs(-x)),
                new Op<>("abs(x+y)", (x, y, z) -> Math.abs(x + y)),
                new Op<>("square -5", (x, y, z) -> 25),
                new Op<>("square (x - y)", (x, y, z) -> (x - y) * (x - y)),
                new Op<>("square -x", (x, y, z) -> x * x),
                new Op<>("square(x+y)", (x, y, z) -> (x + y) * (x + y))
        ));
    }

    public static void main(final String[] args) {
        Util.checkAssert(ParserHardTest.class);
        new ParserHardTest().test();
    }
}
