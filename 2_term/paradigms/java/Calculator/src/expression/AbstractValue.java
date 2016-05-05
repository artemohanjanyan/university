package expression;

import java.util.function.Supplier;

/**
 * Created by artem on 17/03/15.
 */
public class AbstractValue extends AbstractExpression {
    private Supplier<Integer> value;

    public AbstractValue(Supplier<Integer> value) {
        this.value = value;
    }

    @Override
    protected int evaluate() {
        return value.get();
    }
}
