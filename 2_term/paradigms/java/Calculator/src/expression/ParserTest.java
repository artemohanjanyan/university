package expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.UnaryOperator;

import static expression.Util.*;

/**
 * @author Niyaz Nigmatullin
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ParserTest {
    protected final List<Op<UnaryOperator<Integer>>> unary = new ArrayList<>();
    protected final List<List<Op<BinaryOperator<Integer>>>> levels = new ArrayList<>();
    protected List<Op<TripleExpression>> tests;

    protected ParserTest() {
        unary.add(new Op<>("-", a -> -a));

        levels.add(ops(
                new Op<>("+", (a, b) -> a + b),
                new Op<>("-", (a, b) -> a - b)
        ));
        levels.add(ops(
                new Op<>("*", (a, b) -> a * b),
                new Op<>("/", (a, b) -> b == 0 ? null : a / b)
        ));

        tests = ops(
                new Op<TripleExpression>("10", (x, y, z) -> 10),
                new Op<>("x", (TripleExpression) (x, y, z) -> x),
                new Op<>("y", (TripleExpression) (x, y, z) -> y),
                new Op<>("z", (TripleExpression) (x, y, z) -> z),
                new Op<>("x+2", (TripleExpression) (x, y, z) -> x + 2),
                new Op<>("2-y", (TripleExpression) (x, y, z) -> 2 - y),
                new Op<>("  3*  z  ", (TripleExpression) (x, y, z) -> 3 * z),
                new Op<>("x/  -  2", (TripleExpression) (x, y, z) -> -x / 2),
                new Op<>("x*y+(z-1   )/10", (TripleExpression) (x, y, z) -> x * y + (z - 1) / 10),
                new Op<>("-(-(-\t\t-5 + 16   *x*y) + 1 * z) -(((-11)))", (TripleExpression) (x, y, z) -> -(-(5 + 16 * x * y) + z) + 11),
                new Op<>("" + Integer.MAX_VALUE, (TripleExpression) (x, y, z) -> Integer.MAX_VALUE),
                new Op<>("" + Integer.MIN_VALUE, (TripleExpression) (x, y, z) -> Integer.MIN_VALUE),
                new Op<>("x--y--z", (TripleExpression) (x, y, z) -> x + y + z),
                new Op<>("((2+2))-0/(--2)*555", (TripleExpression) (x, y, z) -> 4),
                new Op<>("x-x+y-y+z-(z)", (TripleExpression) (x, y, z) -> 0),
                new Op<>(repeat("(", 500) + "x + y + (-10*-z)" + repeat(")", 500), (TripleExpression) (x, y, z) -> x + y + 10 * z),
                new Op<>("2*" + Integer.MAX_VALUE, (TripleExpression) (x, y, z) -> 2 * Integer.MAX_VALUE)
        );
    }

    @SafeVarargs
    public static <T> List<Op<T>> ops(final Op<T>... ops) {
        return new ArrayList<>(Arrays.asList(ops));
    }

    public static void main(final String[] args) {
        Util.checkAssert(ParserTest.class);
        new ParserTest().test();
    }

    protected void test() {
        for (final Op<TripleExpression> test : tests) {
            System.out.println("Testing: " + test.name);
            final TripleExpression actual = parse(test.name);
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    for (int k = 0; k < 10; k++) {
                        assertEquals(String.format("f(%d, %d, %d)", i, j, k), actual.evaluate(i, j, k), test.f.evaluate(i, j, k));
                    }
                }
            }
        }

        testRandom(1, 2000, (v, i) -> generate(v, i / 5 + 2));
        testRandom(2, 777, (v, i) -> genExpression(1, i / 25 / levels.size() + 1, v, 0));
        System.out.println(total);

        System.out.println("OK");
    }

    public static void testRandom(final int seq, final int n, final BiFunction<int[], Integer, Test> f) {
        System.out.println("Testing random tests #" + seq);
        for (int i = 0; i < n; i++) {
            if (i % 100 == 0) {
                System.out.println("Completed " + i + " out of " + n);
            }
            final int[] vars = new int[]{RNG.nextInt(), RNG.nextInt(), RNG.nextInt()};
            tryTest(vars, f.apply(vars, i));
        }
    }

    static long total;

    private static void tryTest(final int[] vars, final Test test) {
        try {
            try {
                total += test.expr.length();
                final int answer = parse(test.expr).evaluate(vars[0], vars[1], vars[2]);
                assertTrue("division by zero error expected", test.answer != null);
                assertEquals(String.format("f(%d, %d, %d)", vars[0], vars[1], vars[2]), answer, test.answer);
            } catch (final ArithmeticException e) {
                assertTrue("no division by zero in this expression", test.answer == null);
            }
        } catch (final Throwable e) {
            System.out.println("Failed test: " + test.expr);
            throw e;
        }
    }

    private Test generate(final int[] vars, final int depth) {
        if (depth == 0) {
            return constOrVariable(vars);
        }
        final int operator = randomInt(6);
        if (operator <= 0) {
            return genP(vars, depth);
        } else if (operator <= 1) {
            return unary(genP(vars, depth));
        } else {
            return binary(random(levels), genP(vars, depth), genP(vars, depth));
        }
    }

    private Test genP(final int[] vars, final int depth) {
        return p(generate(vars, randomInt(depth)));
    }

    private static Test constOrVariable(final int[] vars) {
        if (RNG.nextBoolean()) {
            final int id = randomInt(3);
            return new Test("xyz".charAt(id) + "", vars[id]);
        } else {
            final int value = RNG.nextInt();
            return new Test(value + "", value);
        }
    }

    private Test genExpression(final int depth, final int coefficient, final int[] vars, final int level) {
        if (level == levels.size()) {
            return genFactor(depth, coefficient, vars);
        } else if (makeNewBranch(depth, coefficient)) {
            return binary(levels.get(level), genExpression(depth + 1, coefficient, vars, level), genExpression(depth, coefficient, vars, level + 1));
        } else {
            return genExpression(depth, coefficient, vars, level + 1);
        }
    }

    private Test genFactor(final int depth, final int coefficient, final int[] vars) {
        if (makeNewBranch(depth, coefficient)) {
            return unary(genFactor(depth + 1, coefficient, vars));
        } else {
            return genValue(depth, coefficient, vars);
        }
    }

    private static Test p(final Test t) {
        return new Test("("  + t.expr + ")", t.answer);
    }

    private static Test binary(final List<Op<BinaryOperator<Integer>>> ops, final Test t1, final Test t2) {
        final Op<BinaryOperator<Integer>> op = random(ops);
        return new Test(t1.expr + op.name + t2.expr, t1.answer == null || t2.answer == null ? null : op.f.apply(t1.answer, t2.answer));
    }

    private Test unary(final Test arg) {
        final Op<UnaryOperator<Integer>> op = random(unary);
        return new Test(op.name + arg.expr, arg.answer == null ? null : op.f.apply(arg.answer));
    }

    private Test genValue(final int depth, final int coefficient, final int[] vars) {
        if (makeNewBranch(depth, coefficient)) {
            return p(genExpression(depth + 1, coefficient, vars, 0));
        } else {
            return constOrVariable(vars);
        }
    }

    private static boolean makeNewBranch(final int depth, final int coefficient) {
        return randomInt(depth + coefficient) < coefficient;
    }

    public static class Test {
        final String expr;
        final Integer answer;

        Test(final String expr, final Integer answer) {
            this.expr = expr;
            this.answer = answer;
        }
    }

    private static TripleExpression parse(final String description) {
        final Parser parser = new ExpressionParser();
        return parser.parse(description);
    }

    protected static final class Op<T> {
        public final String name;
        public final T f;

        public Op(final String name, final T f) {
            this.name = name;
            this.f = f;
        }
    }
}
