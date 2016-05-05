package expression;

/**
 * Created by artem on 17/03/15.
 */
public class AbstractValue extends AbstractExpression {
    @FunctionalInterface
    protected interface Value<T> {
        public T getValue();
    }

    Value<Double> doubleValue;
    Value<Integer> intValue;

    @Override
    protected int intEvaluate() {
        return intValue.getValue();
    }

    @Override
    protected double doubleEvaluate() {
        return doubleValue.getValue();
    }
}
