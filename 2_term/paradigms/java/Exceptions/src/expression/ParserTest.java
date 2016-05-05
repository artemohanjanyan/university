package expression;

import expression.parser.CheckedParser;

import java.util.ArrayList;
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
    protected final List<Op<UnaryOperator<Long>>> unary = new ArrayList<>();
    protected final List<List<Op<BinaryOperator<Long>>>> levels = new ArrayList<>();
    protected List<Op<TExpression>> tests;

    public enum Reason {
        DBZ, OVERFLOW
    }

    protected ParserTest() {
        unary.add(op("-", a -> -a));

        levels.add(list(
                op("+", (a, b) -> a + b),
                op("-", (a, b) -> a - b)
        ));
        levels.add(list(
                op("*", (a, b) -> a * b),
                op("/", (a, b) -> b == 0 ? null : a / b)
        ));

        tests = list(
                op("10", (x, y, z) -> 10L),
                op("x", (x, y, z) -> x),
                op("y", (x, y, z) -> y),
                op("z", (x, y, z) -> z),
                op("x+2", (x, y, z) -> x + 2),
                op("2-y", (x, y, z) -> 2 - y),
                op("  3*  z  ", (x, y, z) -> 3 * z),
                op("x/  -  2", (x, y, z) -> -x / 2),
                op("x*y+(z-1   )/10", (x, y, z) -> x * y + (z - 1) / 10),
                op("-(-(-\t\t-5 + 16   *x*y) + 1 * z) -(((-11)))", (x, y, z) -> -(-(5 + 16 * x * y) + z) + 11),
                op("" + Integer.MAX_VALUE, (x, y, z) -> (long) Integer.MAX_VALUE),
                op("" + Integer.MIN_VALUE, (x, y, z) -> (long) Integer.MIN_VALUE),
                op("x--y--z", (x, y, z) -> x + y + z),
                op("((2+2))-0/(--2)*555", (x, y, z) -> 4L),
                op("x-x+y-y+z-(z)", (x, y, z) -> 0L),
                op(repeat("(", 500) + "x + y + (-10*-z)" + repeat(")", 500), (x, y, z) -> x + y + 10 * z)
        );
    }

    public interface TExpression {
        Long evaluate(long x, long y, long z);
    }

    public static void main(final String[] args) {
        checkAssert(ParserTest.class);
        new ParserTest().test();
    }

    protected void test() {
        for (final Op<TExpression> test : tests) {
            System.out.println("Testing: " + test.name);
            final TripleExpression expression = parse(test.name);
            for (int i = 0; i < 10; i++) {
                for (int j = 0; j < 10; j++) {
                    for (int k = 0; k < 10; k++) {
                        check(new int[]{i, j, k}, expression, lift(test.f.evaluate(i, j, k)));
                    }
                }
            }
        }

        testRandom(1, 2000, (v, i) -> generate(v, i / 5 + 2));
        testRandom(2, 777, (v, i) -> genExpression(1, i / 25 / levels.size() + 1, v, 0));
        System.out.println(total);

        System.out.println("OK");
    }

    protected TripleExpression parse(final String expression) {
        try {
            return new CheckedParser().parse(expression);
        } catch (final Exception e) {
            throw new AssertionError("Parser failed", e);
        }
    }

    public void testRandom(final int seq, final int n, final BiFunction<int[], Integer, Test> f) {
        System.out.println("Testing random tests #" + seq);
        for (int i = 0; i < n; i++) {
            if (i % 100 == 0) {
                System.out.println("Completed " + i + " out of " + n);
            }
            final int[] vars = new int[]{RNG.nextInt(), RNG.nextInt(), RNG.nextInt()};

            final Test test = f.apply(vars, i);
            try {
                total += test.expr.length();
                check(vars, parse(test.expr), test.answer);
            } catch (final Throwable e) {
                System.out.println("Failed test: " + test.expr);
                throw e;
            }
        }
    }

    static long total;

    private void check(final int[] vars, final TripleExpression expression, final Either<Reason, Integer> answer) {
        try {
            final int actual = expression.evaluate(vars[0], vars[1], vars[2]);
            assertTrue(String.format("Error expected x = %d, y=%d, z=%d", vars[0], vars[1], vars[2]), !answer.isLeft());
            assertEquals(String.format("f(%d, %d, %d)\n%s", vars[0], vars[1], vars[2], expression), actual, answer.getRight());
        } catch (final Exception e) {
            if (!answer.isLeft()) {
                throw new AssertionError(String.format("No error expected for x = %d, y=%d, z=%d", vars[0], vars[1], vars[2]), e);
            }
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
            return new Test("xyz".charAt(id) + "", Either.right(vars[id]));
        } else {
            final int value = RNG.nextInt();
            return new Test(value + "", Either.right(value));
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

    private Test binary(final List<Op<BinaryOperator<Long>>> ops, final Test t1, final Test t2) {
        final Op<BinaryOperator<Long>> op = random(ops);
        return new Test(
                t1.expr + op.name + t2.expr,
                t1.answer.flatMapRight(a -> t2.answer.mapRight(b -> op.f.apply((long) a, (long) b)).flatMapRight(this::lift))
        );
    }

    private Test unary(final Test arg) {
        final Op<UnaryOperator<Long>> op = random(unary);
        return new Test(op.name + " " + arg.expr, arg.answer.mapRight(a -> op.f.apply((long) a)).flatMapRight(this::lift));
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

    protected Either<Reason, Integer> lift(final Long value) {
        return value != null ? Either.right(value.intValue()) : Either.left(Reason.DBZ);
    }

    public static class Test {
        final String expr;
        final Either<Reason, Integer> answer;

        Test(final String expr, final Either<Reason, Integer> answer) {
            this.expr = expr;
            this.answer = answer;
        }
    }
}
