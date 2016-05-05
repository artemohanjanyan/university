package expression;

import number.Number;
import number.*;

public abstract class AbstractExpression<T extends Number<T>>
        implements Expression<T>, TripleExpression<T> {

    @Override
    public T evaluate(T x, T y, T z) throws NumberOperationException {
        return evaluate(new VariableContainer<T>()
                .set("x", x).set("y", y).set("z", z));
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
