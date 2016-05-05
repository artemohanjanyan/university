package expression;

import number.*;
import number.Number;

import java.util.function.Function;

public abstract class AbstractValue<T extends Number<T>> extends AbstractExpression<T> {
    private Function<VariableContainer<T>, T> value;

    public AbstractValue(Function<VariableContainer<T>, T> value) {
        this.value = value;
    }

    @Override
    public T evaluate(VariableContainer<T> container) throws NumberOperationException {
        return value.apply(container);
    }




    public static class Const<T extends Number<T>> extends AbstractValue<T> {
        public Const(T value) {
            super((container) -> value);
        }
    }

    public static class Variable<T extends Number<T>> extends AbstractValue<T> {
        public Variable(String name) {
            super((container) -> container.get(name));
        }
    }
}
