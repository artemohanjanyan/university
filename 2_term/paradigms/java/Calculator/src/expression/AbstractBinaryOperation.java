package expression;

import java.util.function.BiFunction;

/**
 * Created by artem on 10/03/15.
 */
public abstract class AbstractBinaryOperation extends AbstractExpression {
    private AbstractExpression left, right;
    private BiFunction<Integer, Integer, Integer> function;

    public AbstractBinaryOperation(AbstractExpression left, AbstractExpression right,
                                      BiFunction<Integer, Integer, Integer> function) {
        this.left = left;
        this.right = right;
        this.function = function;
    }

    @Override
    protected int evaluate() {
        return function.apply(left.evaluate(), right.evaluate());
    }

    @Override
    public String toString() {
        return super.toString() + "(" + left.toString() + ", " + right.toString() + ")";
    }
}
