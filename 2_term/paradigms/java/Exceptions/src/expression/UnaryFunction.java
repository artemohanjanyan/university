package expression;

import number.Number;
import number.NumberOperationException;

import java.util.function.Function;

/**
 * Created by Artem Ohanjanyan on 20/03/15.
 */
public abstract class UnaryFunction extends AbstractExpression {
    private AbstractExpression argument;
    private Function<number.Number, Number> function;

    public UnaryFunction(AbstractExpression argument, Function<Number, Number> function) {
        this.argument = argument;
        this.function = function;
    }

    @Override
    protected Number evaluate() throws NumberOperationException {
        return function.apply(argument.evaluate());
    }

    @Override
    public String toString() {
        return super.toString() + "(" + argument.toString() + ")";
    }

    /**
     * Created by Artem Ohanjanyan on 20/03/15.
     */
    public static class CheckedAbs extends UnaryFunction {
        public CheckedAbs(AbstractExpression argument) {
            super(argument, number.Number::abs);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 29/03/15.
     */
    public static class CheckedNegate extends UnaryFunction {
        public CheckedNegate(AbstractExpression argument) {
            super(argument, number.Number::negate);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 31/03/15.
     */
    public static class CheckedSqrt extends UnaryFunction {
        public CheckedSqrt(AbstractExpression argument) {
            super(argument, number.Number::sqrt);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 24/03/15.
     */
    public static class CheckedSquare extends UnaryFunction {
        public CheckedSquare(AbstractExpression argument) {
            super(argument, number.Number::square);
        }
    }
}
