package expression;

import number.Number;

public interface Expression<T extends Number<T>> {
    T evaluate(VariableContainer<T> variables);
}
