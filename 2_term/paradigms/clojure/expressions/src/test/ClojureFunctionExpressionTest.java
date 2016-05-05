package test;

import expression.Util;

import java.util.Arrays;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static expression.Util.random;
import static expression.Util.randomInt;
import static test.Language.expr;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureFunctionExpressionTest extends BaseTest {
    public static final Dialect UNPARSED = dialect("(variable \"%s\")", "(constant %s.0)", "(%s %s)", "(%s %s %s)");
    public static final Dialect PARSED = dialect("%s", "%s.0", "(%s %s)", "(%s %s %s)");

    protected ClojureFunctionExpressionTest(final Language language, final boolean testMultiarg) {
        super(new ClojureEngine("expression1.clj"), language, true);
        if (testMultiarg) {
            language.tests.addAll(Util.list(
                    m("+", language.vx, language.vy, language.vz),
                    m("+", language.vx),
                    m("-", language.vx, language.vy, language.vz),
                    m("*", language.vx, language.vy, language.vz),
                    m("*", language.vx)
            ));
        }
    }

    @Override
    protected Expr<Double> generate(final double[] vars, final int depth) {
        if (depth == 0 || randomInt(2) == 0) {
            return super.generate(vars, depth);
        }
        @SuppressWarnings("unchecked")
        final Expr<Double>[] as = (Expr<Double>[]) Stream.generate(() -> generateP(vars, depth)).limit(1 + randomInt(5)).toArray(Expr[]::new);
        return m(random(language.bs.get("+"), language.bs.get("*")), as);
    }

    protected ClojureFunctionExpressionTest(final boolean testMultiarg) {
        this(new ExpressionTest.ArithmeticLanguage(UNPARSED, PARSED, ExpressionTest.OPS), testMultiarg);
    }

    @SafeVarargs
    protected final Expr<TExpr> m(final String name, final Expr<TExpr>... as) {
        final Expr<BinaryOperator<Double>> op = language.bs.get(name);
        final BinaryOperator<TExpr> t = (q, r) -> (x, y, z) -> op.answer.apply(q.evaluate(x, y, z), r.evaluate(x, y, z));
        return m(expr(op.parsed, op.unparsed, t), as);
    }

    private <T> Expr<T> m(final Expr<BinaryOperator<T>> op, final Expr<T>[] as) {
        return expr(
                "(" + op.parsed + " " + Arrays.stream(as).map(e -> e.parsed).collect(Collectors.joining(" ")) + ")",
                "(" + op.unparsed + " " + Arrays.stream(as).map(e1 -> e1.unparsed).collect(Collectors.joining(" ")) + ")",
                Arrays.stream(as).map(e2 -> e2.answer).reduce(op.answer).get()
        );
    }

    @Override
    protected String parse(final String expression) {
        return "(parseFunction \"" + expression + "\")";
    }

    public static void main(final String... args) {
        new ClojureFunctionExpressionTest(mode(args, ClojureFunctionExpressionTest.class, "easy", "hard") == 1).test();
    }
}
