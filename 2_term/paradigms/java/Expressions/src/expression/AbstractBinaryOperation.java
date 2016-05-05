package expression;

/**
 * Created by artem on 10/03/15.
 */
public abstract class AbstractBinaryOperation extends AbstractExpression {
    @FunctionalInterface
    protected interface BinaryFunction<T> {
        public T apply(T a, T b);
    }

    AbstractExpression left, right;

    BinaryFunction<Integer> intFunction;
    BinaryFunction<Double> doubleFunction;

    protected AbstractBinaryOperation(AbstractExpression left, AbstractExpression right,
                                      BinaryFunction<Integer> intFunction, BinaryFunction<Double> doubleFunction) {
        this.left = left;
        this.right = right;
        this.intFunction = intFunction;
        this.doubleFunction = doubleFunction;
    }

    @Override
    protected int intEvaluate() {
        return intFunction.apply(left.intEvaluate(), right.intEvaluate());
    }

    @Override
    protected double doubleEvaluate() {
        return doubleFunction.apply(left.doubleEvaluate(), right.doubleEvaluate());
    }
}
