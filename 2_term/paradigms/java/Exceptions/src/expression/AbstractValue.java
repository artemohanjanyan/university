package expression;

import number.Number;
import number.NumberOperationException;

import java.util.function.Supplier;

/**
 * Created by artem on 17/03/15.
 */
public abstract class AbstractValue extends AbstractExpression {
    private Supplier<number.Number> value;

    public AbstractValue(Supplier<Number> value) {
        this.value = value;
    }

    @Override
    protected Number evaluate() throws NumberOperationException {
        return value.get();
    }

    /**
     * Created by artem on 10/03/15.
     */
    public static class Const extends AbstractValue {
        public Const(Number value) {
            super(() -> value);
        }
    }

    /**
     * Created by artem on 10/03/15.
     */
    public static class Variable extends AbstractValue {
        public Variable(String name) {
            super(() -> variables.get(name));
        }
    }
}
