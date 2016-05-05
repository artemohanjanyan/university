package expression;

import number.Number;
import number.NumberOperationException;

import java.util.function.BiFunction;

/**
 * Created by artem on 10/03/15.
 */
public abstract class BinaryFunction extends AbstractExpression {
    private BiFunction<number.Number, Number, Number> function;

    private AbstractExpression left, right;


    public BinaryFunction(AbstractExpression left, AbstractExpression right,
                          BiFunction<Number, Number, Number> function) {
        this.left = left;
        this.right = right;
        this.function = function;
    }

    @Override
    protected Number evaluate() throws NumberOperationException {
        return function.apply(left.evaluate(), right.evaluate());
    }

    @Override
    public String toString() {
        return super.toString() + "(" + left.toString() + ", " + right.toString() + ")";
    }

    /**
     * Created by artem on 10/03/15.
     */
    public static class CheckedAdd extends BinaryFunction {
        public CheckedAdd(AbstractExpression a, AbstractExpression b) {
            super(a, b, number.Number::add);
        }
    }

    /**
     * Created by artem on 10/03/15.
     */
    public static class CheckedDivide extends BinaryFunction {
        public CheckedDivide(AbstractExpression a, AbstractExpression b) {
            super(a, b, number.Number::divide);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 24/03/15.
     */
    public static class CheckedLeftShift extends BinaryFunction {
        public CheckedLeftShift(AbstractExpression left, AbstractExpression right) {
            super(left, right, number.Number::leftShift);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 31/03/15.
     */
    public static class CheckedLog extends BinaryFunction {
        public CheckedLog(AbstractExpression left, AbstractExpression right) {
            super(left, right, number.Number::log);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 24/03/15.
     */
    public static class CheckedMod extends BinaryFunction {
        public CheckedMod(AbstractExpression left, AbstractExpression right) {
            super(left, right, number.Number::modulo);
        }
    }

    /**
     * Created by artem on 10/03/15.
     */
    public static class CheckedMultiply extends BinaryFunction {
        public CheckedMultiply(AbstractExpression a, AbstractExpression b) {
            super(a, b, number.Number::multiply);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 31/03/15.
     */
    public static class CheckedPow extends BinaryFunction {
        public CheckedPow(AbstractExpression left, AbstractExpression right) {
            super(left, right, number.Number::pow);
        }
    }

    /**
     * Created by Artem Ohanjanyan on 24/03/15.
     */
    public static class CheckedRightShift extends BinaryFunction {
        public CheckedRightShift(AbstractExpression left, AbstractExpression right) {
            super(left, right, number.Number::rightShift);
        }
    }

    /**
     * Created by artem on 10/03/15.
     */
    public static class CheckedSubtract extends BinaryFunction {
        public CheckedSubtract(AbstractExpression a, AbstractExpression b) {
            super(a, b, number.Number::subtract);
        }
    }
}
