package test;

import static expression.Util.*;

/**
 * @author Niyaz Nigmatullin
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ModifiedExpressionTest extends ExpressionTest {
    protected ModifiedExpressionTest(final boolean hard) {
        super(hard);
        binary.addAll(list(
                op2("mod", "%", (a, b) -> a % b),
                op2("power", "**", Math::pow)
        ));
        unary.addAll(list(
                op2("abs", "abs", Math::abs),
                op2("log", "log", Math::log)
        ));
        tests.addAll(list(
                op2("abs(subtract(variable('x'), variable('y')))", "x y - abs", (x, y, z) -> Math.abs(x - y)),
                op2("log(add(variable('x'), variable('y')))", "x y + log", (x, y, z) -> Math.log(x + y)),
                op2("mod(variable('x'), variable('y'))", "x y %", (x, y, z) -> x % y)
        ));
    }

    public static void main(final String[] args) {
        checkAssert(ModifiedExpressionTest.class);
        new ModifiedExpressionTest(false).test();
    }
}
