package test;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.DoubleBinaryOperator;
import java.util.function.DoubleUnaryOperator;

import static expression.Util.*;

/**
 * @author Niyaz Nigmatullin
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ExpressionTest {
    public static final int N = 5;

    protected final List<Op2<DoubleUnaryOperator>> unary;
    protected final List<Op2<DoubleBinaryOperator>> binary;
    protected List<Op2<TExpression>> tests;
    protected final ScriptEngine engine;
    protected Result result = new Result();
    private final boolean hard;

    protected ExpressionTest(final boolean hard) {
        this.hard = hard;

        engine = new ScriptEngineManager().getEngineByName("JavaScript");
        init();

        unary = list(
                op2("negate", "negate", a -> -a)
        );

        binary = list(
                op2("add", "+", (a, b) -> a + b),
                op2("subtract", "-", (a, b) -> a - b),
                op2("multiply", "*", (a, b) -> a * b),
                op2("divide", "/", (a, b) -> a / b)
        );

        tests = list(
                op2("cnst(10)","10", (x, y, z) -> 10.0),
                op2("variable('x')", "x", (x, y, z) -> x),
                op2("variable('y')", "y", (x, y, z) -> y),
                op2("variable('z')", "z", (x, y, z) -> z),
                op2("add(variable('x'), cnst(2))", "x 2 +", (x, y, z) -> x + 2),
                op2("subtract(cnst(2), variable('y'))", "2 y -", (x, y, z) -> 2 - y),
                op2("multiply(cnst(3), variable('z'))", "3 z *", (x, y, z) -> 3 * z),
                op2("divide(cnst(3), variable('z'))", "3 z /", (x, y, z) -> 3 / z),
                op2("divide(negate(variable('x')), cnst(2))", "x negate 2 /", (x, y, z) -> -x / 2),
                op2("divide(variable('x'), multiply(variable('y'), variable('z')))", "x y z * /", (x, y, z) -> x / (y * z))
        );
    }

    private void init() {
        engine.put("io", new IO());
        engine.put("result", result);
        try {
            engine.eval(
                    "println = function(message) { io.println(message); };" +
                    "print   = function(message) { io.print  (message); };" +
                    "var expr;" +
                    "var parsed;"
            );
        } catch (final ScriptException e) {
            throw new AssertionError("Invalid initialization", e);
        }
        try {
            engine.eval(new InputStreamReader(new FileInputStream("/home/artem/workspace/study/university/2_sem/paradigms/ecmascript/expressions/src/expression.js"), "UTF-8"));
        } catch (final ScriptException e) {
            throw new AssertionError("Script error", e);
        } catch (final UnsupportedEncodingException e) {
            throw new AssertionError("Fail", e);
        } catch (final FileNotFoundException e) {
            throw new AssertionError("Script not found", e);
        }
    }

    public interface TExpression {
        double evaluate(double x, double y, double z);
    }

    public static void main(final String[] args) {
        checkAssert(ExpressionTest.class);
        new ExpressionTest(false).test();
    }

    protected void test() {
        for (final Op2<TExpression> test : tests) {
            test(test.name, test.f);
            if (hard) {
                test("parse(\"" + test.polish + "\")", test.f);
            }
        }

        testRandom(1, 1000, (v, i) -> generate(v, i / 5 + 2));
        System.out.println(total);

        System.out.println("OK");
    }

    private void test(final String expression, final TExpression f) {
        System.out.println("Testing: " + expression);
        try {
            engine.eval("expr = " + expression);
        } catch (final ScriptException e) {
            throw new AssertionError("Script error", e);
        }
        for (double i = 0; i < N; i += 1) {
            for (double j = 0; j < N; j += 1) {
                for (double k = 0; k < N; k += 1) {
                    try {
                        final double expected = f.evaluate(i, j, k);
                        engine.eval("result.result = expr(" + i + "," + j + "," + k + ");");
                        assertEquals(String.format("f(%f, %f, %f)\n%s", i, j, k, expression), result.result, expected);
                    } catch (final ScriptException e) {
                        throw new AssertionError(String.format("No error expected for x = %f, y = %f, z = %f", i, j, k), e);
                    }
                }
            }
        }
    }

    public void testRandom(final int seq, final int n, final BiFunction<double[], Integer, Test> f) {
        System.out.println("Testing random tests #" + seq);
        for (int i = 0; i < n; i++) {
            if (i % 100 == 0) {
                System.out.println("    Completed " + i + " out of " + n);
            }
            final double[] vars = new double[]{RNG.nextDouble(), RNG.nextDouble(), RNG.nextDouble()};

            final Test test = f.apply(vars, i);
            total += test.expr.length();
            check(vars, op2(test.expr, test.polish, (x, y, z) -> test.answer));
        }
    }

    static long total;

    private void check(final double[] vars, final Op2<TExpression> expression) {
        final double expected = expression.f.evaluate(vars[0], vars[1], vars[2]);
        test(vars, expression.name, expected);
        if (hard) {
            test(vars, "parse(\"" + expression.polish + "\")", expected);
        }
    }

    private void test(final double[] vars, final String expression, final double expected) {
        try {
            engine.eval("result.result = " + expression + "(" + vars[0] + "," + vars[1] + "," + vars[2] + ");");
            assertEquals(String.format("f(%f, %f, %f)\n%s", vars[0], vars[1], vars[2], expression), result.result, expected);
        } catch (final ScriptException e) {
            throw new AssertionError(String.format("No error expected for x = %f, y = %f, z = %f in\n%s", vars[0], vars[1], vars[2], expression), e);
        }
    }

    private Test generate(final double[] vars, final int depth) {
        if (depth == 0) {
            return constOrVariable(vars);
        }
        final int operator = randomInt(6);
        if (operator <= 0) {
            return genP(vars, depth);
        } else if (operator <= 1) {
            return unary(genP(vars, depth));
        } else {
            return binary(binary, genP(vars, depth), genP(vars, depth));
        }
    }

    private Test genP(final double[] vars, final int depth) {
        return generate(vars, randomInt(depth));
    }

    private static Test constOrVariable(final double[] vars) {
        if (RNG.nextBoolean()) {
            final int id = randomInt(3);
            return new Test(
                    "variable('" + "xyz".charAt(id) + "')",
                    "xyz".charAt(id) + "",
                    vars[id]
            );
        } else {
            final int value = RNG.nextInt();
            return new Test("cnst(" + value + ")", value + "", value);
        }
    }

    private Test binary(final List<Op2<DoubleBinaryOperator>> ops, final Test t1, final Test t2) {
        final Op2<DoubleBinaryOperator> op = random(ops);
        return new Test(
                op.name + "(" + t1.expr + ", " + t2.expr + ")",
                t1.polish + " " + t2.polish + " " + op.polish,
                op.f.applyAsDouble(t1.answer, t2.answer)
        );
    }

    private Test unary(final Test arg) {
        final Op2<DoubleUnaryOperator> op = random(unary);
        return new Test(
                op.name + "(" + arg.expr + ")",
                arg.polish + " " + op.polish,
                op.f.applyAsDouble(arg.answer)
        );
    }

    public static class Test {
        final String expr;
        final String polish;
        final double answer;

        Test(final String expr, final String polish, final double answer) {
            this.expr = expr;
            this.polish = polish;
            this.answer = answer;
        }
    }

    public static final class Op2<T> {
        public final String name;
        public final String polish;
        public final T f;

        private Op2(final String name, final String polish, final T f) {
            this.name = name;
            this.polish = polish;
            this.f = f;
        }
    }

    public static <T> Op2<T> op2(final String name, final String polish, final T f) {
        return new Op2<>(name, polish, f);
    }


    public static class IO {
        public void print(final String message) {
            System.out.print(message);
        }

        public void println(final String message) {
            System.out.println(message);
        }
    }

    public static class Result {
        public double result;
    }
}
